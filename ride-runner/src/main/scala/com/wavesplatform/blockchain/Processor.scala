package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.blockchain.BlockchainProcessor.{ProcessResult, Settings}
import com.wavesplatform.blockchain.RestApiScript.LastUpdatedKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.ride.app.RideRunnerMetrics
import com.wavesplatform.ride.app.RideRunnerMetrics.*
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.storage.RequestsStorage
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.utils.ScorexLogging
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import play.api.libs.json.{JsNumber, JsObject}

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.collection.concurrent.TrieMap
import scala.util.chaining.scalaUtilChainingOps

trait Processor {

  /** @return
    *   None if has no block at this height
    */
  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean]

  def removeBlocksFrom(height: Height): Unit

  /** Includes removeBlocksFrom
    * @param toHeight
    */
  def forceRollbackOne(): Unit

  def process(event: BlockchainUpdated): Unit

  def runScripts(forceAll: Boolean = false): Task[Unit]

  def getCachedResultOrRun(address: Address, request: JsObject): Task[JsObject]
}

class BlockchainProcessor(
    settings: Settings,
    blockchainStorage: SharedBlockchainData[RequestKey],
    requestsStorage: RequestsStorage,
    runScriptsScheduler: Scheduler
) extends Processor
    with ScorexLogging {
  private val storage: TrieMap[RequestKey, RestApiScript] = {
    TrieMap.from(requestsStorage.all().map(k => (k, RestApiScript(k._1, blockchainStorage, k._2))))
  }

  // No need for locks, because only runScript can be executed from a different workflow. Other methods are called in sequence
  @volatile private var accumulatedChanges = new ProcessResult[RequestKey]()
  @volatile private var lastEvents         = List.empty[BlockchainUpdated]

  override def process(event: BlockchainUpdated): Unit = {
    val height = Height(event.height)
    accumulatedChanges = event.update match {
      case Update.Empty => accumulatedChanges // Ignore
      case Update.Append(append) =>
        append.body match {
          case Body.Empty         =>
          case _: Body.Block      => lastEvents = List(event)
          case _: Body.MicroBlock => lastEvents = event :: lastEvents
        }
        process(height, append)

      case Update.Rollback(rollback) =>
        lastEvents = lastEvents.dropWhile(x => x.height >= height && x.id != event.id)
        // It wasn't a micro fork, so we have a useful information about changed keys
        if (lastEvents.isEmpty) lastEvents = List(event)
        process(height, rollback)
    }

    // Update this in the end, because a service could be suddenly turned off and we won't know, that we should re-read this block
    blockchainStorage.blockHeaders.update(event)
    log.info(s"Processed $height")
  }

  private def process(h: Height, append: BlockchainUpdated.Append): ProcessResult[RequestKey] = {
    val withUpdatedHeight = {
      // Almost all scripts use the height, so we can run all of them
      // ^ It's not true, I see that unnecessary calls more than regular 10x
      val r =
        if (accumulatedChanges.newHeight == h) accumulatedChanges
        else accumulatedChanges.withAffectedTags(AffectedTags(storage.keySet.toSet)) // TODO

      // TODO Affect scripts if height is increased
//      val r = accumulatedChanges.withAffectedTags(blockchainStorage.taggedHeight.affectedTags)

      r.copy(newHeight = h)
    }

    val (txs, timer) = append.body match {
      // PBBlocks.vanilla(block.getBlock.getHeader)
      case Body.Block(block)           => (block.getBlock.transactions, blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (microBlock.getMicroBlock.getMicroBlock.transactions, microBlockProcessingTime.some)
      case Body.Empty                  => (Seq.empty, none)
    }

    timer match {
      case None => withUpdatedHeight
      case Some(timer) =>
        timer.measure {
          val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
          val txsView     = txs.view.map(_.transaction)
          withUpdatedHeight
            .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.assets.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountBalances.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountLeaseBalances.append(h, x))
            })
            .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.data.append(h, x))
            })
            .pipe(
              txsView
                .flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                      case _                      => none
                    }
                  case _ => none
                }
                .foldLeft(_) { case (r, (pk, script)) =>
                  r.withAffectedTags(blockchainStorage.accountScripts.append(h, pk, script))
                }
            )
            .pipe(
              txsView
                .flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey.toPublicKey).some
                      case _                        => none
                    }
                  case _ => none
                }
                .foldLeft(_) { case (r, (alias, pk)) =>
                  r.withAffectedTags(blockchainStorage.aliases.append(h, alias, pk))
                }
            )
            // We have to do this, otherwise:
            // 1. A transaction could be moved to a new block during NG process
            // 2. We couldn't observe it, e.g. comes in a next micro block or even a block
            // 3. So a script returns a wrong result until the next height, when we re-evaluate all scripts forcefully
            .pipe(append.transactionIds.foldLeft(_) { case (r, txId) =>
              r.withAffectedTags(blockchainStorage.transactions.setHeight(txId, h))
            })
        }
    }
  }

  private def process(h: Height, rollback: BlockchainUpdated.Rollback): ProcessResult[RequestKey] = rollbackProcessingTime.measure {
    // TODO #20 The height will be eventually > if this is a rollback, so we need to run all scripts
    // Almost all scripts use the height
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h)

    blockchainStorage.vrf.removeFrom(h + 1)

    val stateUpdate = rollback.getRollbackStateUpdate
    withUpdatedHeight
      .pipe(stateUpdate.assets.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.assets.rollback(h, x))
      })
      .pipe(stateUpdate.balances.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountBalances.rollback(h, x))
      })
      .pipe(stateUpdate.leasingForAddress.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountLeaseBalances.rollback(h, x))
      })
      .pipe(stateUpdate.dataEntries.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.data.rollback(h, x))
      })
      .pipe(rollback.removedTransactionIds.foldLeft(_) { case (r, txId) =>
        r.withAffectedTags(blockchainStorage.transactions.remove(txId))
      })
    /* TODO #29: Will be fixed (or not) soon with a new BlockchainUpdates API
       NOTE: Ignoring, because 1) almost impossible 2) transactions return to blockchain eventually
      .pipe(stateUpdate.aliases.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.aliases.rollback(h, x))
      })
      .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
        r.withAffectedTags(blockchainStorage.accountScripts.rollback(h, x))
      }) */
  }

  override def runScripts(forceAll: Boolean = false): Task[Unit] = {
    val height = accumulatedChanges.newHeight

    val affectedScripts =
      if (forceAll) storage.values
      else if (accumulatedChanges.affectedScripts.isEmpty) {
        log.debug(s"[$height] Not updated")
        Nil
      } else accumulatedChanges.affectedScripts.flatMap(storage.get)

    // Don't clean all affected scripts, because not all scripts could be added to the storage on the moment of runScripts.
    // See getCachedResultOrRun: it takes some time to run a script and later add it to the storage.
    accumulatedChanges = ProcessResult(affectedScripts = accumulatedChanges.affectedScripts -- affectedScripts.map(_.key))

    val r = Task
      .parTraverseUnordered(affectedScripts)(runScript(_, height = height, hasCaches = true))
      .as(())
      .executeOn(runScriptsScheduler)

    val start = System.nanoTime()
    r.tapEval(_ => Task.now(RideRunnerMetrics.rideScriptRunOnHeightTime(forceAll).update((System.nanoTime() - start).toDouble)))
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = blockchainStorage.blockHeaders.getLocal(height).map(_.id() == id)

  /** Includes removeBlocksFrom
    */
  override def forceRollbackOne(): Unit =
    lastEvents match {
      case Nil => throw new RuntimeException("Can't force rollback one")
      case last :: _ => // a liquid block with same height
        val rollbackToHeight = Height(last.height - 1) // -1 because we undo the lastEvent
        lastEvents.foreach { lastEvent =>
          accumulatedChanges = lastEvent.update match {
            case Update.Append(append) => undo(rollbackToHeight, ByteStr(lastEvent.id.toByteArray), append)
            case _                     => accumulatedChanges
          }
        }

        removeBlocksFrom(Height(last.height))
    }

  private def undo(h: Height, id: ByteStr, append: BlockchainUpdated.Append): ProcessResult[RequestKey] = {
    log.info(s"Undo id=$id to $h")

    // Almost all scripts use the height, so we can run all of them
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h) // TODO #31 Affect all scripts if height is increased

    val (txs, timer) = append.body match {
      case Body.Block(block)           => (block.getBlock.transactions, blockProcessingTime.some)
      case Body.MicroBlock(microBlock) => (microBlock.getMicroBlock.getMicroBlock.transactions, microBlockProcessingTime.some)
      case Body.Empty                  => (Seq.empty, none)
    }

    timer match {
      case None => withUpdatedHeight
      case Some(timer) =>
        timer.measure {
          val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
          val txsView     = txs.view.map(_.transaction)
          withUpdatedHeight
            .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.assets.undoAppend(h, x))
            })
            .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountBalances.undoAppend(h, x))
            })
            .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.accountLeaseBalances.undoAppend(h, x))
            })
            .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
              r.withAffectedTags(blockchainStorage.data.undoAppend(h, x))
            })
            .pipe(
              txsView
                .flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                      case _                      => none
                    }
                  case _ => none
                }
                .foldLeft(_) { case (r, (pk, _)) =>
                  r.withAffectedTags(blockchainStorage.accountScripts.undoAppend(h, pk))
                }
            )
            .pipe(
              txsView
                .flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.CreateAlias(txData) => (txData.alias, tx.senderPublicKey.toPublicKey).some
                      case _                        => none
                    }
                  case _ => none
                }
                .foldLeft(_) { case (r, (alias, _)) =>
                  r.withAffectedTags(blockchainStorage.aliases.undoAppend(h, alias))
                }
            )
            .pipe(append.transactionIds.foldLeft(_) { case (r, txId) =>
              r.withAffectedTags(blockchainStorage.transactions.remove(txId))
            })
        }
    }
  }

  override def removeBlocksFrom(height: Height): Unit = blockchainStorage.blockHeaders.removeFrom(height)

  private def runScript(script: RestApiScript, height: Int, hasCaches: Boolean): Task[JsObject] = {
    val key = script.key
    Task {
      val prev = script.lastResult

      val refreshed = rideScriptRunTime.withTag("has-caches", hasCaches).measure {
        script.refreshed(settings.enableTraces, settings.evaluateScriptComplexityLimit, settings.maxTxErrorLogSize, height, runScriptsScheduler)
      }
      storage.put(key, refreshed)

      val lastResult = refreshed.lastResult
      // TODO comment
//      log.info(f"addr=${key._1}; request=${key._2}; result=$lastResult")
      if ((lastResult \ "error").isEmpty) {
        val complexity = lastResult.value("complexity").as[Int]
        val result     = lastResult.value("result").as[JsObject].value("value")
        log.info(f"result=ok; addr=${key._1}; request=${key._2}; complexity=$complexity")

        prev.value.get("result").map(_.as[JsObject].value("value")).foreach { prevResult =>
          if (result == prevResult) rideScriptUnnecessaryCalls.increment()
          else rideScriptOkCalls.increment()
        }
      } else {
        log.info(f"result=failed; addr=${key._1}; request=${key._2}; errorCode=${(lastResult \ "error").as[Int]}")
        rideScriptFailedCalls.increment()
      }
      lastResult
    }
      .tapError { e => Task(log.error(s"An error during running $key", e)) }
      .executeOn(runScriptsScheduler)
  }

  // To limit duplicated requests
  private val inProcess = new ConcurrentHashMap[RequestKey, Task[JsObject]]()
  override def getCachedResultOrRun(address: Address, request: JsObject): Task[JsObject] = {
    val key = (address, request)
    storage.get(key) match {
      case Some(r) =>
        rideScriptCacheHits.increment()
        Task.now(r.lastResult)

      case None =>
        inProcess.computeIfAbsent(
          key,
          { key =>
            Task {
              rideScriptCacheMisses.increment()
              requestsStorage.append(key)
              blockchainStorage.accountScripts.getUntagged(blockchainStorage.height, address)
            }
              .flatMap {
                case None =>
                  // TODO #19 Change/move an error to an appropriate layer
                  Task.raiseError(ApiException(CustomValidationError(s"Address $address is not dApp")))

                case _ => runScript(RestApiScript(address, blockchainStorage, request), blockchainStorage.height, hasCaches = false)
              }
              .doOnFinish { _ => Task(inProcess.remove(key)) }
          }
        )
    }
  }
}

object BlockchainProcessor {
  case class Settings(enableTraces: Boolean, evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int)

  // TODO #18: don't calculate affectedScripts if all scripts are affected
  private case class ProcessResult[TagT](
      /*allScripts: Set[Int], */ newHeight: Int = 0,
      affectedScripts: Set[TagT] = Set.empty[TagT]
  ) {
    def withAffectedTags(x: AffectedTags[TagT]): ProcessResult[TagT] = copy(affectedScripts = affectedScripts ++ x.xs)
  }
}

case class RestApiScript(address: Address, blockchain: Blockchain, request: JsObject, lastResult: JsObject) {
  def key: RequestKey = (address, request)

  def refreshed(trace: Boolean, evaluateScriptComplexityLimit: Int, maxTxErrorLogSize: Int, height: Int, scheduler: Scheduler): RestApiScript = {
    val result = UtilsApiRoute.evaluate(
      evaluateScriptComplexityLimit,
      Coeval.now(height),
      blockchain,
      address,
      request,
      trace,
      maxTxErrorLogSize
    )
    // TODO restore stateChanges?
    copy(lastResult = result - "stateChanges" + (LastUpdatedKey -> JsNumber(scheduler.clockMonotonic(TimeUnit.MILLISECONDS))))
  }
}

object RestApiScript {
  val LastUpdatedKey = "__lastUpdated"

  def apply(address: Address, blockchainStorage: SharedBlockchainData[RequestKey], request: JsObject): RestApiScript = {
    new RestApiScript(address, new ScriptBlockchain[RequestKey](blockchainStorage, (address, request)), request, JsObject.empty)
  }
}
