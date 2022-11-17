package com.wavesplatform.ride.app

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.blockchain.BlockchainProcessor.RequestKey
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.database.openDB
import com.wavesplatform.grpc.BlockchainGrpcApi.Event
import com.wavesplatform.grpc.{DefaultBlockchainGrpcApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.http.EvaluateApiRoute
import com.wavesplatform.resources.*
import com.wavesplatform.state.Height
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.utils.ScorexLogging
import io.netty.util.concurrent.DefaultThreadFactory
import kamon.instrumentation.executor.ExecutorInstrumentation
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}

import java.io.File
import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Failure

object RideWithBlockchainUpdatesService extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val basePath = args(0)
    val settings = Application.loadApplicationConfig(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    val r = Using.Manager { use =>
      val connector = use(new GrpcConnector)

      log.info("Making gRPC channel to gRPC API...")
      val grpcApiChannel = use(
        connector.mkChannel(
          GrpcClientSettings(
            target = "grpc.wavesnodes.com:6870",
            maxHedgedAttempts = 5,
            maxRetryAttempts = 30,
            keepAliveWithoutCalls = false,
            keepAliveTime = 60.seconds,
            keepAliveTimeout = 15.seconds,
            idleTimeout = 300.days,
            maxInboundMessageSize = 8388608, // 8 MiB
            channelOptions = GrpcClientSettings.ChannelOptionsSettings(
              connectTimeout = 5.seconds
            )
          )
        )
      )

      log.info("Making gRPC channel to Blockchain Updates API...")
      val blockchainUpdatesApiChannel = use(
        connector.mkChannel(
          GrpcClientSettings(
            target = "grpc.wavesnodes.com:6881",
            maxHedgedAttempts = 5,
            maxRetryAttempts = 30,
            keepAliveWithoutCalls = false,
            keepAliveTime = 60.seconds,
            keepAliveTimeout = 15.seconds,
            idleTimeout = 300.days,
            maxInboundMessageSize = 8388608, // 8 MiB
            channelOptions = GrpcClientSettings.ChannelOptionsSettings(
              connectTimeout = 5.seconds
            )
          )
        )
      )

      val commonScheduler = use(
        Executors.newScheduledThreadPool(
          2,
          new ThreadFactoryBuilder().setNameFormat("common-scheduler-%d").setDaemon(false).build()
        )
      )

      val blockchainApi = new DefaultBlockchainGrpcApi(
        settings = DefaultBlockchainGrpcApi.Settings(1.minute),
        grpcApiChannel = grpcApiChannel,
        blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
        hangScheduler = commonScheduler
      )

      val db                = use(openDB(s"$basePath/db"))
      val dbCaches          = new LevelDbPersistentCaches(db)
      val blockchainStorage = new SharedBlockchainData[RequestKey](settings.blockchainSettings, dbCaches, blockchainApi)

      val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
      log.info(s"Current height: $lastHeightAtStart")

      val processor = BlockchainProcessor.mk(blockchainStorage)

      log.info("Warm up caches...") // Also helps to figure out, which data is used by a script
      processor.runScripts(forceAll = true)

      val start             = Height(math.max(0, blockchainStorage.height - 100 - 1))
      val blockchainUpdates = use(blockchainApi.mkBlockchainUpdatesStream())
      val events = blockchainUpdates.stream
        .doOnError(e =>
          Task {
            log.error("Error!", e)
          }
        )
        .takeWhile {
          case _: Event.Next => true
          case Event.Closed =>
            log.info("Blockchain stream closed")
            false
          case Event.Failed(error) =>
            log.error("Blockchain stream failed", error)
            false
        }
        .collect { case Event.Next(event) => event }
        .foldLeftL(BlockchainState.Starting(lastHeightAtStart): BlockchainState)(BlockchainState(processor, _, _))
        .runToFuture(Scheduler(commonScheduler))

      log.info(s"Watching blockchain updates...")
      blockchainUpdates.start(start + 1, lastHeightAtStart + 1) // TODO end

      // ====
      val heavyRequestProcessorPoolThreads =
        settings.restAPISettings.heavyRequestProcessorPoolThreads.getOrElse((Runtime.getRuntime.availableProcessors() * 2).min(4))
      val heavyRequestExecutor = use(
        new ThreadPoolExecutor(
          heavyRequestProcessorPoolThreads,
          heavyRequestProcessorPoolThreads,
          0,
          TimeUnit.MILLISECONDS,
          new LinkedBlockingQueue[Runnable],
          new DefaultThreadFactory("rest-heavy-request-processor", true),
          { (r: Runnable, executor: ThreadPoolExecutor) =>
            log.error(s"$r has been rejected from $executor")
            throw new RejectedExecutionException
          }
        )
      )

      val heavyRequestScheduler = use.acquireWithShutdown(
        Scheduler(
          if (settings.config.getBoolean("kamon.enable"))
            ExecutorInstrumentation.instrument(heavyRequestExecutor, "heavy-request-executor")
          else heavyRequestExecutor,
          ExecutionModel.AlwaysAsyncExecution
        )
      ) { resource =>
        resource.shutdown()
        resource.awaitTermination(5.seconds)
      }

      // TODO ?
//      val routeTimeout = new RouteTimeout(
//        FiniteDuration(settings.config.getDuration("akka.http.server.request-timeout").getSeconds, TimeUnit.SECONDS)
//      )(heavyRequestScheduler)

      val apiRoutes = Seq(new EvaluateApiRoute(settings.restAPISettings, heavyRequestScheduler, processor))

      implicit val actorSystem = use.acquireWithShutdown(ActorSystem("ride-runner", settings.config)) { x =>
        Await.ready(x.terminate(), 20.seconds)
      }
      val httpService = CompositeHttpService(apiRoutes, settings.restAPISettings)
      val httpFuture = Http()
        .newServerAt(settings.restAPISettings.bindAddress, settings.restAPISettings.port)
        .bindFlow(httpService.loggingCompositeRoute)

      val _ = Await.result(httpFuture, 20.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
      Await.result(events, Duration.Inf)
    }

    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info("Done")
    }
  }
}