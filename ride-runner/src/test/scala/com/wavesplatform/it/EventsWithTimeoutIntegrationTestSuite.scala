package com.wavesplatform.it

import com.wavesplatform.events.WrappedEvent
import monix.execution.exceptions.UpstreamTimeoutException

import scala.concurrent.duration.DurationInt

class EventsWithTimeoutIntegrationTestSuite extends BaseIntegrationTestSuite {
  "a transaction is received after a timeout if the previous event is" - {
    "a block append" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        )
      ),
      xGt0 = true
    )

    "a micro block append" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        )
      ),
      xGt0 = true
    )

    "a rollback" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 3,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Next(
          mkRollbackEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 1, initX))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        )
      ),
      xGt0 = true
    )
  }

  "a transaction isn't received after a timeout if the previous event is" - {
    "a block" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)), // Removes the last block, so we didn't see the data update
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 2, 1)) // Resolves a synthetic fork
      ),
      xGt0 = false
    )

    "a micro block" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 2, 2)),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)), // Removes the last block, so we didn't see the data update
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 2, 1)) // Resolves a synthetic fork
        // It's okay, that we don't wait for a next micro block (as on a previous fork), because by default a timeout happens after 90s,
        // so there is a new block probably.
      ),
      xGt0 = false
    )

    "a rollback" - {
      "to a block" in test(
        events = List(
          WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
          WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
          WrappedEvent.Next(
            mkMicroBlockAppendEvent(
              height = 3,
              forkNumber = 1,
              microBlockNumber = 1,
              dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
            )
          ),
          WrappedEvent.Next(
            mkRollbackEvent(
              height = 2,
              forkNumber = 1,
              dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 1, initX))
            )
          ),
          WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
          WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
          // Because a previous height was 3 and we can't switch to a fork with the less height
          WrappedEvent.Next(mkBlockAppendEvent(4, 2)),
          WrappedEvent.Next(mkMicroBlockAppendEvent(4, 2, 1)) // Resolves a synthetic fork
        ),
        xGt0 = false
      )

      "to a micro block" - {
        "tx in a preserved micro block" in test(
          events = List(
            WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
            WrappedEvent.Next(
              mkMicroBlockAppendEvent(
                height = 2,
                forkNumber = 1,
                microBlockNumber = 1,
                dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
              )
            ),
            WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 2)),
            WrappedEvent.Next(mkRollbackEvent(2, 1, 1)),
            WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
            WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
            WrappedEvent.Next(mkMicroBlockAppendEvent(3, 2, 1)) // Resolves a synthetic fork
          ),
          xGt0 = false
        )

        "tx in a removed micro block" in test(
          events = List(
            WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
            WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 1)),
            WrappedEvent.Next(
              mkMicroBlockAppendEvent(
                height = 2,
                forkNumber = 1,
                microBlockNumber = 2,
                dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
              )
            ),
            WrappedEvent.Next(
              mkRollbackEvent(
                height = 2,
                forkNumber = 1,
                microBlockNumber = 1,
                dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 1, initX))
              )
            ),
            WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
            WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
            WrappedEvent.Next(mkMicroBlockAppendEvent(3, 2, 1)) // Resolves a synthetic fork
          ),
          xGt0 = false
        )
      }
    }
  }

  "a transaction wasn't touched after a timeout if it is in" - {
    "a block" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
          )
        ),
        WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)), // Removes the last block, so we didn't see the data update
        WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
        WrappedEvent.Next(mkMicroBlockAppendEvent(3, 2, 1)) // Resolves a synthetic fork
      ),
      xGt0 = true
    )

    "a rollback" - {
      "to a block" in test(
        events = List(
          WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
          WrappedEvent.Next(
            mkBlockAppendEvent(
              height = 2,
              forkNumber = 1,
              dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
            )
          ),
          WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
          WrappedEvent.Next(mkBlockAppendEvent(4, 1)),
          WrappedEvent.Next(mkRollbackEvent(3, 1)),
          WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
          WrappedEvent.Next(mkBlockAppendEvent(4, 2)) // Resolves a synthetic fork
        ),
        xGt0 = true
      )

      "to a micro block" in test(
        events = List(
          WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
          WrappedEvent.Next(
            mkBlockAppendEvent(
              height = 2,
              forkNumber = 1,
              dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX, 1))
            )
          ),
          WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
          WrappedEvent.Next(mkMicroBlockAppendEvent(3, 1, 1)),
          WrappedEvent.Next(mkMicroBlockAppendEvent(3, 1, 2)),
          WrappedEvent.Next(mkRollbackEvent(3, 1, 1)),
          WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
          WrappedEvent.Next(mkBlockAppendEvent(4, 2)) // Resolves a synthetic fork
        ),
        xGt0 = true
      )
    }
  }
}