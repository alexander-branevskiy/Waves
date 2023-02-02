package com.wavesplatform.features

import com.wavesplatform.state.Blockchain

object InvokeScriptSelfPaymentPolicyProvider {
  implicit class SelfPaymentPolicyBlockchainExt(b: Blockchain) {
    def disallowSelfPayment(height: Int = b.height): Boolean =
      b.isFeatureActivated(BlockchainFeatures.BlockV5, height)
  }
}
