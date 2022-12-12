package com.wavesplatform.ride.app

import com.wavesplatform.account.Address
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.database.openDB
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.grpc.{DefaultBlockchainApi, GrpcConnector}
import com.wavesplatform.resources.*
import com.wavesplatform.state.Height
import com.wavesplatform.storage.RequestsStorage
import com.wavesplatform.storage.RequestsStorage.RequestKey
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.utils.ScorexLogging
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import monix.reactive.OverflowStrategy
import play.api.libs.json.{JsObject, Json}
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.{LinkedBlockingQueue, RejectedExecutionException, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.io.Source
import scala.util.Failure

object RideWithBlockchainUpdatesApp extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val startTs                  = System.nanoTime()
    val (globalConfig, settings) = AppInitializer.init(args.headOption.map(new File(_)))

    val r = Using.Manager { use =>
      log.info("Loading args...")
      val inputFile =
        if (args.length < 2) throw new IllegalArgumentException("Please specify an input.json file")
        else new File(args(1))

      use.acquire(new Metrics(globalConfig))

      def mkScheduler(name: String, threads: Int): Scheduler = {
        val executor = use(
          new ThreadPoolExecutor(
            threads,
            threads,
            0,
            TimeUnit.MILLISECONDS,
            new LinkedBlockingQueue[Runnable],
            new DefaultThreadFactory(name, true),
            { (r: Runnable, executor: ThreadPoolExecutor) =>
              log.error(s"$r has been rejected from $executor")
              throw new RejectedExecutionException
            }
          )
        )

        use.acquireWithShutdown(
          Scheduler(
            executor = if (globalConfig.getBoolean("kamon.enable")) ExecutorInstrumentation.instrument(executor, name) else executor,
            executionModel = ExecutionModel.AlwaysAsyncExecution
          )
        ) { resource =>
          resource.shutdown()
          resource.awaitTermination(5.seconds)
        }
      }

      val blockchainEventsStreamScheduler = mkScheduler("blockchain-events", 2)
      val rideScheduler = mkScheduler(
        name = "ride",
        threads = settings.restApi.heavyRequestProcessorPoolThreads.getOrElse((Runtime.getRuntime.availableProcessors() * 2).min(4))
      )

      val scripts = Json
        .parse(use(Source.fromFile(inputFile)).getLines().mkString("\n"))
        .as[List[RequestKey]]

      val connector = use(new GrpcConnector(settings.rideRunner.grpcConnector))

      log.info("Making gRPC channel to gRPC API...")
      val grpcApiChannel = use(connector.mkChannel(settings.rideRunner.grpcApiChannel))

      log.info("Making gRPC channel to Blockchain Updates API...")
      val blockchainUpdatesApiChannel = use(connector.mkChannel(settings.rideRunner.blockchainUpdatesApiChannel))

      val httpBackend = use.acquireWithShutdown(HttpURLConnectionBackend())(_.close())

      val blockchainApi = new DefaultBlockchainApi(
        settings = settings.rideRunner.blockchainApi,
        grpcApiChannel = grpcApiChannel,
        blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
        httpBackend = httpBackend
      )

      val db                = use(openDB(settings.rideRunner.db.directory))
      val dbCaches          = new LevelDbPersistentCaches(db)
      val blockchainStorage = new SharedBlockchainData[RequestKey](settings.blockchain, dbCaches, blockchainApi)

      val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
      log.info(s"Current height: $lastHeightAtStart")

      val processor = new BlockchainProcessor(
        settings.rideRunner.processor,
        blockchainStorage,
        new RequestsStorage {
          override def all(): List[(Address, JsObject)]     = scripts
          override def append(x: (Address, JsObject)): Unit = {} // Ignore, because no way to evaluate a new expr
        },
        rideScheduler
      )

      log.info("Warm up caches...") // Also helps to figure out, which data is used by a script
      Await.result(processor.runScripts(forceAll = true).runToFuture(rideScheduler), Duration.Inf)

      // mainnet
      val lastKnownHeight = Height(3393500)           // math.max(0, blockchainStorage.height - 100 - 1))
      val workingHeight   = Height(lastKnownHeight + 3)
      val endHeight       = Height(workingHeight + 1) // 101 // lastHeightAtStart

      // testnet
      //      val lastKnownHeight = Height(2327973)
      //      val endHeight   = Height(lastKnownHeight + 1)

      val blockchainUpdates = use(blockchainApi.mkBlockchainUpdatesStream(blockchainEventsStreamScheduler))
      val events = blockchainUpdates.stream
        .asyncBoundary(OverflowStrategy.BackPressure(4))
        .doOnError(e => Task { log.error("Error!", e) })
        .takeWhile {
          case WrappedEvent.Next(_) => true
          case WrappedEvent.Closed =>
            log.info("Blockchain stream closed")
            false
          case WrappedEvent.Failed(error) =>
            log.error("Blockchain stream failed", error)
            false
        }
        .collect { case WrappedEvent.Next(event) => event }
        .scanEval(Task.now[BlockchainState](BlockchainState.Starting(workingHeight)))(BlockchainState(processor, _, _))
        .lastL
        .runToFuture(Scheduler(blockchainEventsStreamScheduler))

      log.info(s"Watching blockchain updates...")
      blockchainUpdates.start(lastKnownHeight + 1, endHeight)

      Await.result(events, Duration.Inf)
    }

    val duration = System.nanoTime() - startTs
    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info(f"Done in ${duration / 1e9d}%5f s")
    }
  }
}