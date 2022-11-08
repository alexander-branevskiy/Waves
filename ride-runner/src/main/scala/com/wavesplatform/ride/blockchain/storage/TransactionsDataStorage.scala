package com.wavesplatform.ride.blockchain.storage

import cats.syntax.option.*
import com.google.protobuf.ByteString
import com.wavesplatform.api.grpc.*
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction
import com.wavesplatform.ride.blockchain.DataKey.TransactionDataKey
import com.wavesplatform.ride.blockchain.caches.BlockchainCaches
import com.wavesplatform.ride.blockchain.{AppendResult, BlockchainData, DataKey}
import com.wavesplatform.state.{Height, TransactionId, TxMeta}
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{EthereumTransaction, Transaction}

class TransactionsDataStorage[TagT](caches: BlockchainCaches, blockchainApi: BlockchainGrpcApi)
    extends DataStorage[TransactionId, (TxMeta, Option[Transaction]), TagT] {
  override def mkDataKey(key: TransactionId): DataKey = TransactionDataKey(key)

  override def getFromBlockchain(key: TransactionId): Option[(TxMeta, Option[Transaction])] = blockchainApi.getTransferLikeTransaction(key)

  override def getFromPersistentCache(maxHeight: Int, key: TransactionId): BlockchainData[(TxMeta, Option[Transaction])] =
    caches.getTransaction(key) // TODO maxHeight?

  override def setPersistentCache(height: Int, key: TransactionId, data: BlockchainData[(TxMeta, Option[Transaction])]): Unit =
    caches.setTransaction(key, data) // TODO height?

  override def removeFromPersistentCache(fromHeight: Int, key: TransactionId): BlockchainData[(TxMeta, Option[Transaction])] =
    caches.removeTransaction(key) // TODO height?

  def getWithTransferLike(height: Int, key: TransactionId, tag: TagT): Option[(TxMeta, Option[TransferTransactionLike])] =
    get(height, key, tag).map { case (meta, maybeTx) =>
      val tx = maybeTx.flatMap {
        case tx: TransferTransactionLike => tx.some
        case tx: EthereumTransaction =>
          tx.payload match {
            case payload: EthereumTransaction.Transfer =>
              // tx.toTransferLike()
              // payload.toTransferLike(tx, this).toOption
              none
            case _ => none
          }
        case _ => none
      }
      (meta, tx)
    }

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def append(height: Int, pbTxId: ByteString, signedTx: SignedTransaction): AppendResult[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    super.append(
      height,
      txId,
      (
        TxMeta(
          height = Height(height),
          succeeded = true,
          spentComplexity = 0
        ),
        // TODO: copy-paste from BlockchainGrpcApi
        signedTx.toVanilla.toOption.flatMap {
          case etx: EthereumTransaction =>
            etx.payload match {
              case _: EthereumTransaction.Transfer =>
                // TODO have to call GET /eth/assets/... or blockchain.resolveERC20Address
                // transfer.toTransferLike(tx, blockchain = ???).toOption
                none
              case _ => none
            }

          case tx: TransferTransactionLike => tx.some
          case _                           => none
        }
      ).some
    )
  }
}
