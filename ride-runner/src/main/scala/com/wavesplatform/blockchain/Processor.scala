package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.CustomValidationError
import com.wavesplatform.api.http.ApiException
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.blockchain.BlockchainProcessor.{ProcessResult, RequestKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.storage.DataKey
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import play.api.libs.json.JsObject

import scala.collection.concurrent.TrieMap
import scala.util.chaining.scalaUtilChainingOps

trait Processor {

  /** @return
    *   None if has no block at this height
    */
  def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean]
  def removeFrom(height: Height): Unit
  def process(event: BlockchainUpdated): Unit
  def runScripts(forceAll: Boolean = false): Unit

  // TODO move to another place?
  def getLastResultOrRun(address: Address, request: JsObject): Task[JsObject]
}

class BlockchainProcessor private (
    blockchainStorage: SharedBlockchainData[RequestKey],
    private val storage: TrieMap[RequestKey, RestApiScript]
) extends Processor
    with ScorexLogging {
  @volatile private var accumulatedChanges = new ProcessResult[RequestKey]()

  override def process(event: BlockchainUpdated): Unit = {
    val height = Height(event.height)
    accumulatedChanges = event.update match {
      case Update.Empty              => accumulatedChanges // Ignore
      case Update.Append(append)     => process(height, append)
      case Update.Rollback(rollback) => process(height, rollback)
    }

    // Update this in the end, because a service could be suddenly turned off and we won't know, that we should re-read this block
    blockchainStorage.blockHeaders.update(event)
  }

  private def process(h: Height, append: BlockchainUpdated.Append): ProcessResult[RequestKey] = {
    // TODO the height will be eventually > if this is a rollback
    // Almost all scripts use the height
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h) // TODO

    val txs = append.body match {
      // PBBlocks.vanilla(block.getBlock.getHeader)
      case Body.Block(block)           => block.getBlock.transactions
      case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
      case Body.Empty                  => Seq.empty
    }

    val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
    val txsView     = txs.view.map(_.transaction)
    withUpdatedHeight
      .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.assets.append(h, x))
      })
      .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.portfolios.append(h, x))
      })
      .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.portfolios.append(h, x))
      })
      .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
        r.withAppendResult(blockchainStorage.data.append(h, x))
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
            r.withAppendResult(blockchainStorage.accountScripts.append(h, pk, script))
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
            r.withAppendResult(blockchainStorage.aliases.append(h, alias, pk))
          }
      )
      .pipe(append.transactionIds.view.zip(txs).foldLeft(_) { case (r, (txId, tx)) =>
        r.withAppendResult(blockchainStorage.transactions.append(h, txId, tx))
      })
  }

  private def process(h: Height, rollback: BlockchainUpdated.Rollback): ProcessResult[RequestKey] = {
    // TODO the height will be eventually > if this is a rollback
    // Almost all scripts use the height
    val withUpdatedHeight = accumulatedChanges.copy(newHeight = h) // TODO

    val stateUpdate = rollback.getRollbackStateUpdate
    withUpdatedHeight
      .pipe(stateUpdate.assets.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.assets.rollback(h, x))
      })
      /* TODO:
      .pipe(stateUpdate.aliases.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.aliases.rollback(h, x))
      })*/
      .pipe(stateUpdate.balances.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.portfolios.rollback(h, x))
      })
      .pipe(stateUpdate.leasingForAddress.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.portfolios.rollback(h, x))
      })
      .pipe(stateUpdate.dataEntries.foldLeft(_) { case (r, x) =>
        r.withRollbackResult(blockchainStorage.data.rollback(h, x))
      })
    /* TODO:
        .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
          r.withRollbackResult(blockchainStorage.accountScripts.rollback(h, x))
        })
        // TODO Remove?
        .pipe(append.transactionIds.view.zip(txs).foldLeft(_) { case (r, txId) =>
          r.withRollbackResult(blockchainStorage.transactions.rollback(h, txId)
        })*/

  }

  override def runScripts(forceAll: Boolean = false): Unit = {
    val height = accumulatedChanges.newHeight
    if (accumulatedChanges.uncertainKeys.nonEmpty) {
      log.debug(s"Getting data for keys: ${accumulatedChanges.uncertainKeys.toVector.map(_.toString).sorted.mkString(", ")}")
      accumulatedChanges.uncertainKeys.foreach(_.reload(height))
    }

    // log.info(
    //   s"==> $h >= $lastHeightAtStart, started: $started, affectedScripts: ${curr.affectedScripts}, batchedEvents for heights: {${batchedEvents
    //     .map(_.getUpdate.height)
    //     .mkString(", ")}}"
    // )

    if (forceAll) storage.values.foreach(runScript(height, _))
    else if (accumulatedChanges.affectedScripts.isEmpty) log.debug(s"[$height] Not updated")
    else accumulatedChanges.affectedScripts.flatMap(storage.get).foreach(runScript(height, _))

    accumulatedChanges = ProcessResult()
  }

  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = blockchainStorage.blockHeaders.getLocal(height).map(_.id() == id)

  override def removeFrom(height: Height): Unit = blockchainStorage.blockHeaders.removeFrom(height)

  private def runScript(height: Int, script: RestApiScript): Unit = {
    val refreshed = script.refreshed
    val key       = script.key
    storage.put(key, refreshed)
    log.info(s"[$height, $key] apiResult: ${refreshed.lastResult.value("result").as[JsObject].value("value")}")
  }

  override def getLastResultOrRun(address: Address, request: JsObject): Task[JsObject] = {
    val key = (address, request)
    storage.get(key) match {
      case Some(r) => Task.now(r.lastResult)
      case None =>
        blockchainStorage.accountScripts.getUntagged(blockchainStorage.height, address) match {
          case None =>
            // TODO should not be in business logic
            Task.raiseError(ApiException(CustomValidationError(s"Address $address is not dApp")))

          case _ =>
            // TODO settings.evaluateScriptComplexityLimit = 52000
            val script = RestApiScript(address, blockchainStorage, request).refreshed
            storage.putIfAbsent(key, script)
            Task.now(script.lastResult)
        }
    }
  }
}

object BlockchainProcessor {
  type RequestKey = (Address, JsObject)

  def mk(blockchainStorage: SharedBlockchainData[RequestKey], scripts: List[RequestKey] = Nil): BlockchainProcessor =
    new BlockchainProcessor(blockchainStorage, TrieMap.from(scripts.map(k => (k, RestApiScript(k._1, blockchainStorage, k._2)))))

  // TODO get rid of TagT
  // TODO don't calculate affectedScripts if all scripts are affected
  private case class ProcessResult[TagT](
      /*allScripts: Set[Int], */ newHeight: Int = 0,
      affectedScripts: Set[TagT] = Set.empty[TagT],
      uncertainKeys: Set[DataKey] = Set.empty[DataKey]
  ) {
    def withAppendResult(x: AppendResult[TagT]): ProcessResult[TagT] =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = x.mayBeChangedKey.foldLeft(uncertainKeys)(_ - _)
      )

    def withRollbackResult(x: RollbackResult[TagT]): ProcessResult[TagT] =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = uncertainKeys ++ x.mayBeUncertainKey
      )
  }
}

case class RestApiScript(address: Address, blockchain: Blockchain, request: JsObject, lastResult: JsObject) {
  def key: RequestKey = (address, request)

  def refreshed: RestApiScript = {
    // TODO settings.evaluateScriptComplexityLimit = 52000
    // TODO enable / disable traces
    val result = UtilsApiRoute.evaluate(52000, blockchain, address, request, trace = true)
    copy(lastResult = result)
  }
}

object RestApiScript {
  def apply(address: Address, blockchainStorage: SharedBlockchainData[RequestKey], request: JsObject): RestApiScript = {
    new RestApiScript(address, new ScriptBlockchain[RequestKey](blockchainStorage, (address, request)), request, JsObject.empty)
  }
}
