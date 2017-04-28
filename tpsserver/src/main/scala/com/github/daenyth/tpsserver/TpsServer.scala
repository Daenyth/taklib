package com.github.daenyth.tpsserver

import com.github.daenyth.taklib.{Game, MoveResult, PtnParser}
import fs2.Task
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.{HttpService, Response}

import scalaz.ValidationNel
import scalaz.syntax.applicative._

case class TpsMove(tps: String, move: String)

object TpsServer {
  def takeTurn(move: TpsMove): ValidationNel[String, MoveResult[Game]] = {
    val gameE = Game.fromTps(move.tps).leftMap(err => s"TPS: $err").validationNel
    val moveE = PtnParser
      .parseEither(PtnParser.turnAction, move.move)
      .leftMap(err => s"Move: $err")
      .validationNel
    (gameE |@| moveE) apply {
      case (game, action) => game.takeTurn(action)
    }
  }

  private def runTpsMove(move: TpsMove): Task[Response] =
    takeTurn(move)
      .fold(
        err => BadRequest(Json.obj("errors" -> err.list.toVector.asJson)),
        ok => Ok(ok.asJson)
      )

  val tpsService = HttpService {
    case req @ POST -> Root / "tpsMove" =>
      req.as(jsonOf[TpsMove]).flatMap(runTpsMove)
  }

  implicit val gameEncoder: Encoder[Game] = Encoder[String].contramap(game => game.toTps)
  implicit def moveResultEncoder[A: Encoder]: Encoder[MoveResult[A]] = deriveEncoder
}
