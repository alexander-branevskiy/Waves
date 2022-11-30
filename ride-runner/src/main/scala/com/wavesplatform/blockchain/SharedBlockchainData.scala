package com.wavesplatform.blockchain

import cats.syntax.option.*
import com.wavesplatform.features.EstimatorProvider
import com.wavesplatform.grpc.DefaultBlockchainApi
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.storage.*
import com.wavesplatform.storage.persistent.PersistentCaches
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class SharedBlockchainData[TagT](val settings: BlockchainSettings, persistentCaches: PersistentCaches, blockchainApi: DefaultBlockchainApi)
    extends ScorexLogging {
  private val chainId = settings.addressSchemeCharacter.toByte

  val data = new AccountDataStorage[TagT](blockchainApi, persistentCaches.accountDataEntries)

  val accountScripts = new AccountScriptStorage[TagT](chainId, estimator, blockchainApi, persistentCaches.accountScripts)

  val blockHeaders = new BlockHeadersStorage(blockchainApi, persistentCaches.blockHeaders)

  val vrf = new VrfStorage(blockchainApi, persistentCaches.vrf, height)

  // Ride: wavesBalance, height, lastBlock
  def height: Int = blockHeaders.last.height

  // No way to get this from blockchain updates
  var activatedFeatures =
    load[Unit, Map[Short, Int]](
      _ => persistentCaches.getActivatedFeatures(),
      _ => blockchainApi.getActivatedFeatures(height).some,
      (_, xs) => xs.mayBeValue.foreach(persistentCaches.setActivatedFeatures)
    )(())
      .getOrElse(throw new RuntimeException("Impossible: activated features are empty"))

  val assets = new AssetStorage[TagT](blockchainApi, persistentCaches.assetDescriptions)

  val aliases = new AliasStorage[TagT](chainId, blockchainApi, persistentCaches.aliases)

  val accountBalances = new AccountBalanceStorage[TagT](blockchainApi, persistentCaches.accountBalances)

  val accountLeaseBalances = new AccountLeaseBalanceStorage[TagT](blockchainApi, persistentCaches.accountLeaseBalances)

  val transactions = new TransactionsStorage[TagT](blockchainApi, persistentCaches.transactions)

  private def estimator: ScriptEstimator = EstimatorProvider.byActivatedFeatures(settings.functionalitySettings, activatedFeatures, height)

  private def load[KeyT, ValueT](
      fromCache: KeyT => RemoteData[ValueT],
      fromBlockchain: KeyT => Option[ValueT],
      updateCache: (KeyT, RemoteData[ValueT]) => Unit
  )(key: KeyT): Option[ValueT] =
    fromCache(key)
      .or(RemoteData.loaded(fromBlockchain(key)).tap(updateCache(key, _)))
      .mayBeValue
}