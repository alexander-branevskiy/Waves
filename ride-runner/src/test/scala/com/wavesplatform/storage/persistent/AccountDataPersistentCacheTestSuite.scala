package com.wavesplatform.storage.persistent

import com.wavesplatform.state.{BooleanDataEntry, DataEntry}
import com.wavesplatform.storage.AccountDataKey

class AccountDataPersistentCacheTestSuite extends PersistentCacheTestSuite[AccountDataKey, DataEntry[?]] {
  private val defaultPairDataKey      = "foo"
  protected override val defaultKey   = (alice.publicKey.toAddress, defaultPairDataKey)
  protected override val defaultValue = BooleanDataEntry(defaultPairDataKey, value = true)

  protected override def test(f: PersistentCache[AccountDataKey, DataEntry[?]] => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.accountDataEntries)
  }
}
