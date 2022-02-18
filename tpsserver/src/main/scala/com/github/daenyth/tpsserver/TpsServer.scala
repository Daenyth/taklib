package com.github.daenyth.tpsserver

import cats.data.ValidatedNel
import cats.syntax.all._
import com.github.daenyth.taklib.{Game, MoveResult, PtnParser}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s._
import cats.effect.IO
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import io.circe.Decoder
import scala.annotation.nowarn

case class TpsMove(tps: String, move: String)

object TpsServer {
  def takeTurn(move: TpsMove): ValidatedNel[String, MoveResult[Game]] = {
    val gameE = Game.fromTps(move.tps).leftMap(err => s"TPS: $err").toValidatedNel
    val moveE = PtnParser
      .parseEither(PtnParser.turnAction, move.move)
      .leftMap(err => s"Move: $err")
      .toValidatedNel
    (gameE, moveE).mapN((game, action) => game.takeTurn(action))
  }

  private def runTpsMove(move: TpsMove): IO[Response[IO]] =
    takeTurn(move)
      .fold(
        err => BadRequest(Json.obj("errors" -> err.toList.toVector.asJson)),
        ok => Ok(ok.asJson)
      )

  val tpsService = HttpRoutes.of[IO] { case req @ POST -> Root / "tpsMove" =>
    req.as[TpsMove].flatMap(runTpsMove)
  }

  private implicit val tpsMoveDecoder: Decoder[TpsMove] = deriveDecoder
  private implicit val gameEncoder: Encoder[Game] = Encoder[String].contramap(game => game.toTps)
  @nowarn("msg=never used")
  private implicit def moveResultEncoder[A: Encoder]: Encoder[MoveResult[A]] = {
    import io.circe.generic.auto._
    deriveEncoder
  }
}
