package com.wavesplatform.ride.app

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.settings.*
import com.wavesplatform.utils.{Misconfiguration, ScorexLogging, forceStopApplication}

import java.io.File
import scala.util.{Failure, Success, Try}

object AppInitializer extends ScorexLogging {
  def init(externalConfig: Option[File] = None): (Config, RideRunnerGlobalSettings) = {
    val maybeExternalConfig = Try(
      externalConfig.map(f => ConfigFactory.parseFile(f.getAbsoluteFile, ConfigParseOptions.defaults().setAllowMissing(false)))
    )
    val config = loadConfig(maybeExternalConfig.getOrElse(None))

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("waves.directory", config.getString("waves.directory"))
    if (config.hasPath("waves.config.directory")) System.setProperty("waves.config.directory", config.getString("waves.config.directory"))

    maybeExternalConfig match {
      case Success(None) =>
        val currentBlockchainType = Try(ConfigFactory.defaultOverrides().getString("waves.blockchain.type"))
          .orElse(Try(ConfigFactory.defaultOverrides().getString("waves.defaults.blockchain.type")))
          .map(_.toUpperCase)
          .getOrElse("TESTNET")

        log.warn(s"Config file not defined, default $currentBlockchainType config will be used")

      case Failure(exception) =>
        log.error(s"Couldn't read ${externalConfig.get.toPath.toAbsolutePath}", exception)
        forceStopApplication(Misconfiguration)

      case _ => // Pass
    }

    val settings = RideRunnerGlobalSettings.fromRootConfig(config)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchain.addressSchemeCharacter.toByte
    }

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    //    if (config.getBoolean("kamon.enable")) {
    //      Kamon.init(config)
    //    } else {
    //      Kamon.reconfigure(config)
    //    }
    //
    //    sys.addShutdownHook {
    //      Try(Await.result(Kamon.stop(), 30 seconds))
    //    }

    (config, settings)
  }
}