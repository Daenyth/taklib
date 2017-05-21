package com.github.daenyth.tpsserver

import cats.data.ValidatedNel
import cats.syntax.either._
import cats.syntax.cartesian._
import com.github.daenyth.taklib.{Game, MoveResult, PtnParser}
import fs2.Task
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.{HttpService, Response}

case class TpsMove(tps: String, move: String)

object TpsServer {
  def takeTurn(move: TpsMove): ValidatedNel[String, MoveResult[Game]] = {
    val gameE = Game.fromTps(move.tps).leftMap(err => s"TPS: $err").toValidatedNel
    val moveE = PtnParser
      .parseEither(PtnParser.turnAction, move.move)
      .leftMap(err => s"Move: $err")
      .toValidatedNel
    (gameE |@| moveE) map {
      case (game, action) => game.takeTurn(action)
    }
  }

  private def runTpsMove(move: TpsMove): Task[Response] =
    takeTurn(move)
      .fold(
        err => BadRequest(Json.obj("errors" -> err.toList.toVector.asJson)),
        ok => Ok(ok.asJson)
      )

  val tpsService = HttpService {
    case req @ POST -> Root / "tpsMove" =>
      req.as(jsonOf[TpsMove]).flatMap(runTpsMove)
  }

  implicit val gameEncoder: Encoder[Game] = Encoder[String].contramap(game => game.toTps)
  implicit def moveResultEncoder[A: Encoder]: Encoder[MoveResult[A]] = deriveEncoder
}
