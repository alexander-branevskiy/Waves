package com.wavesplatform.ride.input

import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.*
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.transaction.Asset

case class RunnerAccountState(
    scriptInfo: Option[RunnerScriptInfo] = None,
    data: Map[String, RunnerDataEntry] = Map.empty,
    hasData: Option[Boolean] = None,
    balance: Map[Asset, Long] = Map.empty,
    leasing: Option[LeaseBalance] = None,
    balanceHistory: Map[Int, Map[Option[BlockId], Seq[BalanceSnapshot]]] = Map.empty,
    aliases: List[Alias] = Nil
)

case class RunnerScriptInfo(
    publicKey: PublicKey,
    script: Script
)

sealed trait RunnerDataEntry {
  def toDataEntry(key: String): DataEntry
}

// Note, play-json can't find descendants in the companion object

case class BinaryRunnerDataEntry(value: ByteStr) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = BinaryDataEntry(key, value)
}

case class BooleanRunnerDataEntry(value: Boolean) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = BooleanDataEntry(key, value)
}

case class IntegerRunnerDataEntry(value: Long) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = IntegerDataEntry(key, value)
}

case class StringRunnerDataEntry(value: String) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = StringDataEntry(key, value)
}
