package com.wavesplatform.state.diffs.invoke

import cats.Id
import cats.implicits.toFlatMapOps
import cats.syntax.either.*
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EvaluatorFixProvider.*
import com.wavesplatform.lang.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, Log, ScriptResult}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.metrics.*
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.BalanceDiffValidation
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.smart.script.trace.CoevalR
import com.wavesplatform.transaction.smart.script.trace.CoevalR.traced

object InvokeScriptDiff {
  private val stats = TxProcessingStats

  def apply(
      blockchain: Blockchain,
      blockTime: Long,
      limitedExecution: Boolean,
      totalComplexityLimit: Int,
      remainingComplexity: Int,
      remainingCalls: Int,
      remainingActions: Int,
      remainingBalanceActionsV6: Int,
      remainingAssetActionsV6: Int,
      remainingPayments: Int,
      remainingData: Int,
      remainingDataSize: Int,
      calledAddresses: Set[Address],
      invocationRoot: DAppEnvironment.InvocationTreeTracker
  )(
      tx: InvokeScript
  ): CoevalR[(Diff, EVALUATED, Int, Int, Int, Int, Int, Int)] = ???
}
