package com.wavesplatform.history

import scala.concurrent.duration._

import cats.syntax.option._
import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.{AddressPortfolio, AddressTransactions, CommonBlocksApi}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.database
import com.wavesplatform.database.{DBExt, Keys, LevelDBWriter}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.{BlockchainUpdater, _}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.SystemTime
import org.iq80.leveldb.DB

case class Domain(db: DB, blockchainUpdater: BlockchainUpdaterImpl, levelDBWriter: LevelDBWriter) {
  import Domain._

  @volatile
  var triggers: Seq[BlockchainUpdateTriggers] = Nil

  val posSelector: PoSSelector = PoSSelector(blockchainUpdater, None)

  def blockchain: BlockchainUpdaterImpl = blockchainUpdater

  def lastBlock: Block = {
    blockchainUpdater.lastBlockId
      .flatMap(blockchainUpdater.liquidBlock)
      .orElse(levelDBWriter.lastBlock)
      .getOrElse(TestBlock.create(Nil))
  }

  def liquidDiff: Diff =
    blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty)

  def effBalance(a: Address): Long = blockchainUpdater.effectiveBalance(a, 1000)

  def appendBlock(b: Block): Seq[Diff] = blockchainUpdater.processBlock(b).explicitGet()

  def removeAfter(blockId: ByteStr): DiscardedBlocks = blockchainUpdater.removeAfter(blockId).explicitGet()

  def appendMicroBlock(b: MicroBlock): BlockId = blockchainUpdater.processMicroBlock(b).explicitGet()

  def lastBlockId: ByteStr = blockchainUpdater.lastBlockId.get

  def carryFee: Long = blockchainUpdater.carryFee

  def balance(address: Address): Long               = blockchainUpdater.balance(address)
  def balance(address: Address, asset: Asset): Long = blockchainUpdater.balance(address, asset)

  def nftList(address: Address): Seq[(IssuedAsset, AssetDescription)] = db.withResource { resource =>
    AddressPortfolio
      .nftIterator(resource, address, blockchainUpdater.bestLiquidDiff.orEmpty, None, blockchainUpdater.assetDescription)
      .toSeq
  }

  def addressTransactions(address: Address, from: Option[ByteStr] = None): Seq[(Height, Transaction)] =
    AddressTransactions
      .allAddressTransactions(
        db,
        blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
        address,
        None,
        Set.empty,
        from
      )
      .map { case (h, tx, _) => h -> tx }
      .toSeq

  def portfolio(address: Address): Seq[(IssuedAsset, Long)] = Domain.portfolio(address, db, blockchainUpdater)

  def appendBlock(txs: Transaction*): Block = {
    val block = createBlock(Block.PlainBlockVersion, txs)
    appendBlock(block)
    lastBlock
  }

  def appendKeyBlock(): Block = {
    val block = createBlock(Block.NgBlockVersion, Nil)
    appendBlock(block)
    lastBlock
  }

  def appendMicroBlock(txs: Transaction*): Unit = {
    val lastBlock = this.lastBlock
    val block     = lastBlock.copy(transactionData = lastBlock.transactionData ++ txs)
    val signature = com.wavesplatform.crypto.sign(defaultSigner.privateKey, block.bodyBytes())
    val mb        = MicroBlock.buildAndSign(lastBlock.header.version, defaultSigner, txs, blockchainUpdater.lastBlockId.get, signature).explicitGet()
    blockchainUpdater.processMicroBlock(mb).explicitGet()
  }

  def rollbackTo(height: Int): Unit = {
    val blockId = blockchain.blockId(height).get
    blockchainUpdater.removeAfter(blockId).explicitGet()
  }

  def rollbackMicros(offset: Int = 1): Unit = {
    val blockId =
      blockchainUpdater.microblockIds
        .drop(offset)
        .headOption
        .getOrElse(throw new IllegalStateException("Insufficient count of microblocks"))

    blockchainUpdater.removeAfter(blockId).explicitGet()
  }

  def createBlock(version: Byte, txs: Seq[Transaction], ref: Option[ByteStr] = blockchainUpdater.lastBlockId, strictTime: Boolean = false): Block = {
    val reference = ref.getOrElse(randomSig)
    val parent = ref.flatMap { bs =>
      val height = blockchain.heightOf(bs)
      height.flatMap(blockchain.blockHeader).map(_.header)
    } getOrElse (lastBlock.header)

    val grandParent = ref.flatMap { bs =>
      val height = blockchain.heightOf(bs)
      height.flatMap(h => blockchain.blockHeader(h - 1)).map(_.header)
    }

    val timestamp =
      if (blockchain.height > 0)
        parent.timestamp + posSelector
          .getValidBlockDelay(blockchain.height, defaultSigner, parent.baseTarget, blockchain.balance(defaultSigner.toAddress) max 1e12.toLong)
          .explicitGet()
      else
        System.currentTimeMillis() - (1 hour).toMillis

    val consensus =
      if (blockchain.height > 0)
        posSelector
          .consensusData(
            defaultSigner,
            blockchain.height,
            settings.blockchainSettings.genesisSettings.averageBlockDelay,
            parent.baseTarget,
            parent.timestamp,
            grandParent.map(_.timestamp),
            timestamp
          )
          .explicitGet()
      else NxtLikeConsensusBlockData(60, generationSignature)

    Block
      .buildAndSign(
        version = if (consensus.generationSignature.size == 96) Block.ProtoBlockVersion else version,
        timestamp = if (strictTime) timestamp else SystemTime.getTimestamp(),
        reference = reference,
        baseTarget = consensus.baseTarget,
        generationSignature = consensus.generationSignature,
        txs = txs,
        featureVotes = Nil,
        rewardVote = -1L,
        signer = defaultSigner
      )
      .explicitGet()
  }

  val blocksApi: CommonBlocksApi = {
    def loadBlockMetaAt(db: DB, blockchainUpdater: BlockchainUpdaterImpl)(height: Int): Option[BlockMeta] =
      blockchainUpdater.liquidBlockMeta.filter(_ => blockchainUpdater.height == height).orElse(db.get(Keys.blockMetaAt(Height(height))))

    def loadBlockInfoAt(db: DB, blockchainUpdater: BlockchainUpdaterImpl)(
        height: Int
    ): Option[(BlockMeta, Seq[(Transaction, Boolean)])] =
      loadBlockMetaAt(db, blockchainUpdater)(height).map { meta =>
        meta -> blockchainUpdater
          .liquidTransactions(meta.id)
          .orElse(db.readOnly(ro => database.loadTransactions(Height(height), ro)))
          .fold(Seq.empty[(Transaction, Boolean)])(identity)
      }

    CommonBlocksApi(blockchainUpdater, loadBlockMetaAt(db, blockchainUpdater), loadBlockInfoAt(db, blockchainUpdater))
  }
}

object Domain {
  implicit class BlockchainUpdaterExt[A <: BlockchainUpdater](bcu: A) {
    def processBlock(block: Block): Either[ValidationError, Seq[Diff]] =
      bcu.processBlock(block, block.header.generationSignature)
  }

  def portfolio(address: Address, db: DB, blockchainUpdater: BlockchainUpdaterImpl): Seq[(IssuedAsset, Long)] = db.withResource { resource =>
    AddressPortfolio
      .assetBalanceIterator(resource, address, blockchainUpdater.bestLiquidDiff.orEmpty, id => blockchainUpdater.assetDescription(id).exists(!_.nft))
      .toSeq
  }
}
