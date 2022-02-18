package com.github.daenyth.tpsserver

import com.github.daenyth.taklib.TpsParser
import io.circe.Json
import io.circe.generic.auto._
import io.circe.optics.JsonPath.root
import io.circe.syntax._
import org.http4s.syntax.all._
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.{Method, Request}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class TpsServerTest extends AsyncFlatSpec with Matchers with OptionValues {
  "TpsService" should "Run a valid request" in {
    val move = TpsMove("x6/x6/x6/x6/x6/x6 1 1", "a1").asJson
    val request = Request[IO](Method.POST, uri"/tpsMove").withEntity(move)
    val task = for {
      respose <- TpsServer.tpsService.orNotFound.run(request)
      js <- respose.as[Json]
    } yield {
      val newTps = root.OkMove.nextState.string.getOption(js).value
      TpsParser.parseEither(TpsParser.tps, newTps) shouldBe Symbol("right")
    }
    task.unsafeToFuture()
  }
}
