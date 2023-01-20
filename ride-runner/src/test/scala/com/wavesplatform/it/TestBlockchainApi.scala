package com.wavesplatform.it

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.grpc.BalanceResponse
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.{AssetDescription, DataEntry, Height}
import com.wavesplatform.transaction.Asset
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

class TestBlockchainApi(implicit val scheduler: Scheduler) extends BlockchainApi {
  val blockchainUpdatesUpstream = ConcurrentSubject.publish[WrappedEvent[SubscribeEvent]]

  override def mkBlockchainUpdatesStream(scheduler: Scheduler): BlockchainApi.BlockchainUpdatesStream =
    new BlockchainApi.BlockchainUpdatesStream {
      override val downstream: Observable[WrappedEvent[SubscribeEvent]] = blockchainUpdatesUpstream

      override def start(fromHeight: Int, toHeight: Int): Unit = {}

      override def closeUpstream(): Unit   = blockchainUpdatesUpstream.onComplete()
      override def closeDownstream(): Unit = {}

      override def close(): Unit = {
        closeUpstream()
        closeDownstream()
      }
    }

  override def getCurrentBlockchainHeight(): Int = kill("getCurrentBlockchainHeight")

  override def getActivatedFeatures(height: Int): Map[Short, Int] = kill(s"getActivatedFeatures(height=$height)")

  override def getAccountDataEntries(address: Address): Seq[DataEntry[?]]               = kill(s"getAccountDataEntries(address=$address)")
  override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] = kill(s"getAccountDataEntry(address=$address, $key)")
  override def getAccountScript(address: Address): Option[Script]                       = kill(s"getAccountScript(address=$address)")
  override def getBlockHeader(height: Int): Option[SignedBlockHeader]                   = kill(s"getBlockHeader(height=$height)")

  override def getBlockHeaderRange(fromHeight: Int, toHeight: Int): List[SignedBlockHeader] =
    kill(s"getBlockHeaderRange(fromHeight=$fromHeight, toHeight=$toHeight)")

  override def getVrf(height: Int): Option[ByteStr]                                    = kill(s"getVrf(height=$height)")
  override def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription] = kill(s"getAssetDescription(asset=$asset)")
  override def resolveAlias(alias: Alias): Option[Address]                             = kill(s"resolveAlias(alias=$alias)")
  override def getBalance(address: Address, asset: Asset): Long                        = kill(s"getBalance(address=$address, asset=$asset)")
  override def getLeaseBalance(address: Address): BalanceResponse.WavesBalances        = kill(s"getLeaseBalance(address=$address)")
  override def getTransactionHeight(id: ByteStr): Option[Height]                       = kill(s"getTransactionHeight(id=$id)")

  private def kill(call: String) = throw new RuntimeException(call)
}