package com.wavesplatform.ride

import cats.syntax.either.*
import com.wavesplatform.Application
import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.{ConflictedRequestStructure, InvalidMessage}
import com.wavesplatform.api.http.requests.byteStrFormat
import com.wavesplatform.api.http.utils.{UtilsEvaluator, UtilsInvocationRequest}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.lang.{API, ValidationError}
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  Blockchain,
  DataEntry,
  LeaseBalance,
  TxMeta,
  VolumeAndFee
}
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, GenericError, ScriptExecutionError}
import com.wavesplatform.transaction.smart.script.trace.TraceStep
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import play.api.libs.json.*

import java.io.File
import scala.io.Source
import scala.util.{Try, Using}

object RideRunner {
  /*
  seed: test

  Nonce is: 0
  Public key: Cq5itmx4wbYuogySAoUp58MimLLkQrFFLr1tpJy2BYp1
  Address in 'W': 3PCH3sUqeiPFAhrKzEnSEXoE2B6G9YNromV

  Nonce is: 1
  Public key: BWfushcMzh4YhHUjaHAW4iPUJHtCZ6SrpkDXtEhAiRQn
  Address in 'W': 3P6GhtTsABtYUgzhXTA4cDwbqqy7HqruiQQ

  Nonce is: 2
  Public key: 9K1Nu1udY4NAv77ktLqGAAxRtkL1epGA7tickpjDgPjP
  Address in 'W': 3PE7TH41wVuhn2SpAwWBBzeGxxzz8wXrb6L

  Nonce is: 3
  Public key: 5gmbkRC62E4YMX5RAnotUtpqccna8wPaNqCqo5hZsTeo
  Address in 'W': 3P4xDBqzXgR8HyXoyNn1C8Bd88h4rsEBMHA
   */
  def main(args: Array[String]): Unit = {
    val basePath     = args(0)
    val nodeSettings = Application.loadApplicationConfig(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    val input          = Json.parse(Using(Source.fromFile(new File(s"$basePath/input.json")))(_.getLines().mkString("\n")).get).as[RideRunnerInput]
    val scriptSrc      = """
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo(x: Int) = {
  let alice = Address(base58'3P6GhtTsABtYUgzhXTA4cDwbqqy7HqruiQQ')
  let carl = addressFromRecipient(Alias("carl"))
  let bob = Address(base58'3PE7TH41wVuhn2SpAwWBBzeGxxzz8wXrb6L')
  let jane = Address(base58'3P4xDBqzXgR8HyXoyNn1C8Bd88h4rsEBMHA')

  let asset = base58'8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS'
  let txId = base58'8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq'

  let x1 = getIntegerValue(alice, "a")
  let x2 = if (isDataStorageUntouched(carl)) then 1 else 0
  let x3 = assetBalance(bob, asset)
  let x4 = value(assetInfo(asset)).decimals
  let x5 = value(blockInfoByHeight(3296627)).height
  let x6 = size(value(scriptHash(this)))
  let x7 = value(transactionHeightById(txId))
  let x8 = value(transferTransactionById(txId)).amount
  let x9 = wavesBalance(carl).available
  let x10 = invoke(this, "bar", [], []).exactAs[Int]
  ([], x + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10)
}

@Callable(inv)
func bar() = {
  let x1 = if (value(getBoolean("b"))) then 1 else 2
  ([], x1)
}"""
    val estimator      = ScriptEstimatorV3(fixOverflow = true, overhead = false)
    val compiledScript = API.compile(input = scriptSrc, estimator).explicitGet()

    def kill(methodName: String) = throw new RuntimeException(methodName)
    // TODO default values?
    val blockchain: Blockchain = new Blockchain {
      // conflicts with accountData
      override def hasData(address: Address): Boolean = input.hasData.getOrElse(address, throw new RuntimeException(s"hasData($address)"))

      override def transactionInfo(id: BlockId) = kill("transactionInfo")

      override def accountScript(address: Address): Option[AccountScriptInfo] = {
        // 3PCH3sUqeiPFAhrKzEnSEXoE2B6G9YNromV
        input.accountScript.get(address).map { input =>
          input.copy(
            script = Script.fromBase64String(Base64.encode(compiledScript.bytes)).explicitGet(),
            verifierComplexity = compiledScript.verifierComplexity,
            complexitiesByEstimator = Map(
              estimator.version -> compiledScript.callableComplexities
            )
          )
        }
      }

      override def blockHeader(height: Int): Option[SignedBlockHeader] = input.blockHeader.get(height)

      override def hitSource(height: Int): Option[BlockId] = input.hitSource.get(height) // VRF

      /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
      override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
        input.balanceSnapshots
          .getOrElse(address, throw new RuntimeException(s"address: $address")) // TODO Map.empty)
          .getOrElse(from, throw new RuntimeException(s"from: $from"))          // Map.empty)
          .getOrElse(to, throw new RuntimeException(s"to: $to"))                // Seq.empty)

      override def hasAccountScript(address: Address) = kill("hasAccountScript")

      override def settings: BlockchainSettings = nodeSettings.blockchainSettings

      override def height: Int = input.height

      override def score: BigInt = kill("score")

      override def carryFee: Long = kill("carryFee")

      override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

      /** Features related */
      override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

      override def activatedFeatures: Map[Short, Int] = input.activatedFeatures

      override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

      override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

      override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = input.assetDescription.get(id)

      override def resolveAlias(a: Alias): Either[ValidationError, Address] =
        input.resolveAlias.get(a).toRight(AliasDoesNotExist(a): ValidationError)

      override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

      override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

      /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
      override def accountData(acc: Address, key: String): Option[DataEntry[_]] =
        input.accountData(acc).get(key)

      override def leaseBalance(address: Address): LeaseBalance = input.leaseBalance.getOrElse(address, LeaseBalance(0, 0))

      override def balance(address: Address, mayBeAssetId: Asset): Long =
        input.balance.get(address).flatMap(_.get(mayBeAssetId)).getOrElse(0)

      override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
        input.transferById.get(id).map { tx =>
          val meta = transactionMeta(id).getOrElse(throw new RuntimeException(s"Can't find a metadata of the transaction $id"))
          (meta.height, tx)
        }

      /** Block reward related */
      override def blockReward(height: Int): Option[Long] = kill("blockReward")

      override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

      override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

      override def transactionMeta(id: ByteStr): Option[TxMeta] = input.transactionMeta.get(id)

      override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

      override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = kill("assetScript")

      override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")
    }

    val trace         = input.trace
    val request       = input.request
    val scriptAddress = input.scriptAddress
    val scriptInfo    = blockchain.accountScript(scriptAddress).get
    val pk            = scriptInfo.publicKey
    val script        = scriptInfo.script

    val simpleExpr = request.value.get("expr").map(parseCall(_, script.stdLibVersion))
    val exprFromInvocation =
      request
        .asOpt[UtilsInvocationRequest]
        .map(_.toInvocation.flatMap(UtilsEvaluator.toExpr(script, _)))

    val exprE = (simpleExpr, exprFromInvocation) match {
      case (Some(_), Some(_)) if request.fields.size > 1 => Left(ConflictedRequestStructure.json)
      case (None, None)                                  => Left(InvalidMessage.json)
      case (Some(expr), _)                               => Right(expr)
      case (None, Some(expr))                            => Right(expr)
    }

    val apiResult = Try(exprE.flatMap { exprE =>
      val evaluated = for {
        expr <- exprE
        limit = Int.MaxValue // settings.evaluateScriptComplexityLimit
        (result, complexity, log) <- UtilsEvaluator.executeExpression(blockchain, script, scriptAddress, pk, limit)(expr)
      } yield {
        Json.obj(
          "result"     -> ScriptValuesJson.serializeValue(result),
          "complexity" -> complexity
        ) ++ (if (trace) Json.obj(TraceStep.logJson(log)) else Json.obj())
      }
      evaluated.leftMap {
        case e: ScriptExecutionError => Json.obj("error" -> ApiError.ScriptExecutionError.Id, "message" -> e.error)
        case e                       => ApiError.fromValidationError(e).json
      }
    }.merge)

    println(s"apiResult: $apiResult")
  }

  // UtilsApiRoute
  private def parseCall(js: JsReadable, version: StdLibVersion) = {
    val binaryCall = js
      .asOpt[ByteStr]
      .toRight(GenericError("Unable to parse expr bytes"))
      .flatMap(bytes => SerdeV1.deserialize(bytes.arr).bimap(GenericError(_), _._1))

    val textCall = js
      .asOpt[String]
      .toRight(GenericError("Unable to read expr string"))
      .flatMap(UtilsEvaluator.compile(version))

    binaryCall.orElse(textCall)
  }
}