package com.wavesplatform.storage

import com.wavesplatform.account.Address
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.LeaseBalance
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache

class AccountLeaseBalanceStorage[TagT](blockchainApi: BlockchainApi, override val persistentCache: PersistentCache[Address, LeaseBalance])
    extends ExactWithHeightStorage[Address, LeaseBalance, TagT] {
  override def getFromBlockchain(key: Address): Option[LeaseBalance] = {
    val r = blockchainApi.getLeaseBalance(key)
    Some(LeaseBalance(r.leaseIn, r.leaseOut))
  }

  def append(height: Int, update: StateUpdate.LeasingUpdate): AppendResult[TagT] = {
    val address = update.address.toAddress
    append(height, address, toVanilla(update))
  }

  // TODO #21 Copy-paste from append
  def rollback(rollbackHeight: Int, update: StateUpdate.LeasingUpdate): RollbackResult[TagT] = {
    val address = update.address.toAddress
    rollback(rollbackHeight, address, toVanilla(update))
  }

  private def toVanilla(x: StateUpdate.LeasingUpdate): LeaseBalance = LeaseBalance(x.inAfter, x.outAfter)
}