package com.wavesplatform.api.http.transaction

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.transaction.TransactionsApiRoute.LeaseStatus
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.InvokeScriptResult.{Lease, LeaseCancel}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import play.api.libs.json.*
import play.api.libs.json.JsonConfiguration.Aux

private[transaction] final case class LeaseRef(
    id: ByteStr,
    originTransactionId: Option[ByteStr],
    sender: Option[Address],
    recipient: Option[Address],
    amount: Option[Long],
    height: Option[Int],
    status: LeaseStatus,
    cancelHeight: Option[Int],
    cancelTransactionId: Option[ByteStr]
)

private[transaction] object LeaseRef {
  implicit val config: Aux[Json.MacroOptions] = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)
  implicit val byteStrWrites: Format[ByteStr] = com.wavesplatform.utils.byteStrFormat
  implicit val jsonWrites: OWrites[LeaseRef]  = Json.writes[LeaseRef]

  private def create(
      blockchain: Blockchain,
      leaseId: ByteStr,
      height: Option[Int] = None,
      cancelId: Option[ByteStr] = None,
      sourceId: Option[ByteStr] = None,
      senderAddressB: Option[ByteStr] = None,
      amount: Option[Long] = None,
      recipient: Option[Address] = None
  ): LeaseRef = {
    val details          = blockchain.leaseDetails(leaseId)
    val txMeta           = details.flatMap(d => blockchain.transactionMeta(d.sourceId))
    val detailsRecipient = details.flatMap(d => blockchain.resolveAlias(d.recipient).toOption)
    val senderAddress    = senderAddressB.flatMap(b => Address.fromBytes(b.arr).toOption)

    val (status, cancelHeight, cancelTxId) = details.map(_.status) match {
      case Some(LeaseDetails.Status.Active) | None           => (true, None, None)
      case Some(LeaseDetails.Status.Cancelled(height, txId)) => (false, Some(height), txId)
      case Some(LeaseDetails.Status.Expired(height))         => (false, Some(height), None)
    }

    LeaseRef(
      leaseId,
      details.map(_.sourceId).orElse(sourceId),
      details.map(_.sender.toAddress).orElse(senderAddress),
      detailsRecipient.orElse(recipient),
      details.map(_.amount).orElse(amount),
      txMeta.map(_.height).orElse(height),
      LeaseStatus(status),
      cancelHeight,
      cancelTxId.orElse(cancelId)
    )
  }

  def fromLease(lease: Lease, blockchain: Blockchain): LeaseRef = {
    val recipient = blockchain.resolveAlias(lease.recipient).toOption
    create(blockchain, lease.id, lease.height, None, lease.invokeId, lease.senderAddress, Some(lease.amount), recipient)
  }

  def fromLeaseCancel(cancel: LeaseCancel, blockchain: Blockchain): LeaseRef = {
    val recipient = cancel.recipient.flatMap(a => Address.fromBytes(a.arr).toOption)
    create(blockchain, cancel.id, cancel.height, cancel.invokeId, cancel.sourceId, cancel.senderAddress, cancel.amount, recipient)
  }

  def fromLeaseCancelTransaction(leaseCancel: LeaseCancelTransaction, blockchain: Blockchain): LeaseRef =
    create(blockchain, leaseCancel.leaseId)
}
