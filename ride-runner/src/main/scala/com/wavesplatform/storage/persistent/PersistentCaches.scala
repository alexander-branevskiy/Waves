package com.wavesplatform.storage.persistent

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, LeaseBalance}
import com.wavesplatform.storage.{AccountAssetKey, AccountDataKey}
import com.wavesplatform.transaction.Asset

trait PersistentCaches {
  def accountDataEntries: PersistentCache[AccountDataKey, DataEntry[?]]
  def accountScripts: PersistentCache[Address, AccountScriptInfo]
  def assetDescriptions: PersistentCache[Asset.IssuedAsset, AssetDescription]
  def aliases: PersistentCache[Alias, Address]
  def accountBalances: PersistentCache[AccountAssetKey, Long]
  def accountLeaseBalances: PersistentCache[Address, LeaseBalance]
  def transactions: TransactionPersistentCache
  def blockHeaders: BlockPersistentCache
  def vrf: VrfPersistentCache

  def getActivatedFeatures(): RemoteData[Map[Short, Int]]
  def setActivatedFeatures(data: Map[Short, Int]): Unit
}