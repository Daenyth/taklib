package com.github.daenyth.takcli

import cats.effect.IO
import com.github.daenyth.taklib._

import scala.io.StdIn
import scala.util.control.NoStackTrace
import cats.syntax.all._
import cats.effect.IOApp
import cats.effect.ExitCode

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    mainT
      .recoverWith { case CleanExit =>
        IO.println("Exiting")
      }
      .as(ExitCode.Success)

  def mainT: IO[Unit] = printStartup >> getInitialGame >>= runGameLoop

  def getInitialGame: IO[Game] = promptSize.flatMap {
    Game.ofSize(_) match {
      case scala.Left(err)   => IO(println(err)) >> getInitialGame
      case scala.Right(game) => IO(game)
    }
  }

  def runGameLoop(g: Game): IO[Unit] = runGameTurn(g).flatMap {
    case OkMove(nextState) =>
      runGameLoop(nextState)
    case InvalidMove(reason) =>
      IO(println(s"Bad move: $reason")) >> runGameLoop(g)
    case GameOver(result, finalState) =>
      printGame(finalState) >> endGame(result)
  }

  def endGame(end: GameEndResult): IO[Unit] = IO {
    println("Game over!")
    println(end)
  }

  def runGameTurn(g: Game): IO[MoveResult[Game]] =
    printGame(g) >> promptAction.map(g.takeTurn)

  def printGame(g: Game) = IO {
    val nextPlayInfo = g.turnNumber match {
      case 1               => "White to play (Black stone)"
      case 2               => "Black to play (White stone)"
      case n if n % 2 == 0 => "Black to play"
      case _               => "White to play"
    }
    println(s"Move ${g.turnNumber} - $nextPlayInfo")
    print(pretty(g))
    println()
  }

  def printStartup = IO(println("TakCLI"))

  def promptSize: IO[Int] =
    IO {
      print("Game size?\n  > ")
      StdIn.readInt()
    }.recoverWith { case n: NumberFormatException =>
      IO(println(s"Bad size: $n")) >> promptSize
    }

  def pretty(g: Game): String =
    g.currentBoard.boardPositions
      .map(_.reverse)
      .transpose
      .map(_.map(_.toTps).mkString("\t"))
      .mkString("\n")

  def promptAction: IO[TurnAction] =
    IO(StdIn.readLine("Your Move?\n  > "))
      .flatMap { input =>
        if (input == null) { throw CleanExit }
        else
          PtnParser
            .parseEither(PtnParser.turnAction, input)
            .fold(
              err => IO.raiseError(PtnParseError(err)),
              ta => IO.pure(ta)
            )
      }
      .recoverWith { case PtnParseError(err) =>
        IO(println(s"Bad move: $err")) >> promptAction
      }
}

case class PtnParseError(msg: String) extends Exception(msg) with NoStackTrace
case object CleanExit extends Exception with NoStackTrace
