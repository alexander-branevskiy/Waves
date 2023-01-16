package com.wavesplatform.blockchain

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.state.Height
import monix.eval.Task
import play.api.libs.json.JsObject

class TestProcessor extends Processor {
  override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = None

  override def getCachedResultOrRun(address: Address, request: JsObject): Task[JsObject] = Task.raiseError(new RuntimeException("Test"))

  /** Includes removeBlocksFrom
    */
  override def forceRollbackOne(): Unit = {}

  override def removeBlocksFrom(height: Height): Unit = {
    actions = actions.appended(RemoveFrom(height))
  }

  override def process(event: BlockchainUpdated): Unit = {
    actions = actions.appended(Process(event))
  }

  override def runScripts(forceAll: Boolean): Task[Unit] = {
    actions = actions.appended(RunScripts(forceAll))
    Task.now(())
  }

  var actions: Vector[ProcessorAction] = Vector.empty

  override def toString: String = s"TestProcessor(${actions.mkString(", ")})"
}

sealed trait ProcessorAction extends Product with Serializable

case class RemoveFrom(height: Int) extends ProcessorAction

object RemoveFrom {
  def apply(event: SubscribeEvent): RemoveFrom = RemoveFrom(event.getUpdate.height)
  def next(event: SubscribeEvent): RemoveFrom  = RemoveFrom(event.getUpdate.height + 1)
}

case class Process(updated: BlockchainUpdated) extends ProcessorAction
object Process {
  def apply(event: SubscribeEvent): Process = Process(event.getUpdate)
}

case class RunScripts(forceAll: Boolean) extends ProcessorAction