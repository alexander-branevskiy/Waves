package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object EvaluatorFixProvider {
  implicit class CorrectFunctionCallScopeExt(b: Blockchain) {
    def correctFunctionCallScope: Boolean =
      b.height >= b.settings.functionalitySettings.estimatorSumOverflowFixHeight

    def newEvaluatorMode: Boolean = newEvaluatorMode(b.height)
    def newEvaluatorMode(height: Int): Boolean =
      b.isFeatureActivated(BlockchainFeatures.RideV6, height)
  }
}
