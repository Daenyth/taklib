package com.github.daenyth.tpsserver

import com.github.daenyth.taklib.{Game, MoveResult, PtnParser}
import fs2.Task
import io.circe.Encoder
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.{HttpService, Response}

import scalaz.\/
import scalaz.syntax.applicative._

case class TpsMove(tps: String, move: String)

object TpsServer {
  def takeTurn(move: TpsMove): String \/ MoveResult[Game] =
    (Game.fromTps(move.tps) |@| PtnParser.parseEither(PtnParser.turnAction, move.move)) apply {
      case (game, toAction) =>
        game.takeTurn(toAction(game.nextPlayer))
    }

  private def runTpsMove(move: TpsMove): Task[Response] =
    takeTurn(move).map(_.asJson).fold(BadRequest(_), Ok(_))

  val tpsService = HttpService {
    case req @ POST -> Root / "tpsMove" =>
      req.as(jsonOf[TpsMove]).flatMap(runTpsMove)
  }

  implicit val gameEncoder: Encoder[Game] = Encoder[String].contramap(game => game.toTps)
  implicit def moveResultEncoder[A: Encoder]: Encoder[MoveResult[A]] = deriveEncoder
}
