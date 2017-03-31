package com.github.daenyth.tpsserver

import com.github.daenyth.taklib.TpsParser
import io.circe.Json
import io.circe.generic.auto._
import io.circe.optics.JsonPath.root
import io.circe.syntax._
import org.http4s.Uri.uri
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.{Method, Request}
import org.scalatest.{AsyncFlatSpec, Matchers, OptionValues}

class TpsServerTest extends AsyncFlatSpec with Matchers with OptionValues {
  "TpsService" should "Run a valid request" in {
    val move = TpsMove("x6/x6/x6/x6/x6/x6 1 1", "a1").asJson
    val task = for {
      request <- Request(Method.POST, uri("/tpsMove")).withBody(move)
      respose <- TpsServer.tpsService.run(request).map(_.orNotFound)
      js <- respose.as[Json]
    } yield {
      val newTps = root.OkMove.nextState.string.getOption(js).value
      TpsParser.parseEither(TpsParser.tps, newTps) shouldBe 'right
    }
    task.unsafeRunAsyncFuture()
  }
}
