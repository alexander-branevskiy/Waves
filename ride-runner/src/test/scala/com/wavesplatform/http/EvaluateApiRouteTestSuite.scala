package com.wavesplatform.http

import akka.http.scaladsl.model.HttpRequest
import com.wavesplatform.account.Address
import com.wavesplatform.blockchain.EmptyProcessor
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.wallet.Wallet
import io.netty.util.HashedWheelTimer
import monix.eval.Task
import play.api.libs.json.*

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.DurationInt

class EvaluateApiRouteTestSuite extends RouteSpec("/utils") with RestAPISettingsHelper {
  private val default     = Wallet.generateNewAccount("test".getBytes(StandardCharsets.UTF_8), 0)
  private val defaultAddr = default.toAddress

  "EvaluateApiRoute" - {
    "POST /utils/script/evaluate/{address}" - {
      val processor = new EmptyProcessor() {
        override def getLastResultOrRun(address: Address, request: JsObject): Task[JsObject] =
          if (address == defaultAddr)
            Task.now(
              Json.obj(
                "result" -> Json.obj(
                  "type"  -> "Int",
                  "value" -> 2
                ),
                "complexity" -> 0,
                "vars"       -> Json.arr(),
                "expr"       -> "1 + 1",
                "address"    -> defaultAddr.toString
              )
            )
          else super.getLastResultOrRun(address, request)
      }

      val api = EvaluateApiRoute(
        Schedulers.timeBoundedFixedPool(
          new HashedWheelTimer(),
          5.seconds,
          1,
          "rest-time-limited"
        ),
        processor
      )
      val route = seal(api.route)

      def evalScript(text: String, trace: Boolean): HttpRequest =
        Post(s"/utils/script/evaluate/$defaultAddr${if (trace) "?trace=true" else ""}", Json.obj("expr" -> text))

      "without traces" in evalScript("1 + 1", trace = false) ~> route ~> check {
        responseAs[JsObject] \\ "vars" shouldBe empty
      }

      "with traces" in evalScript("1 + 1", trace = true) ~> route ~> check {
        responseAs[JsObject] \\ "vars" should not be empty
      }
    }
  }
}