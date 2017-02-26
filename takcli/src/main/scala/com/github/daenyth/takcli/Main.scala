package com.github.daenyth.takcli

import com.github.daenyth.taklib._

import scala.io.StdIn
import scala.util.control.NoStackTrace
import scalaz.concurrent.Task
import scalaz.syntax.monad._

object Main {
  def main(args: Array[String]): Unit =
    mainT.handleWith {
      case CleanExit => Task.now(println("Exiting"))
    }.unsafePerformSync

  def mainT: Task[Unit] = printStartup *> getInitialGame >>= runGameLoop

  def getInitialGame: Task[Game] =
    promptSize.map(Game.ofSize).handleWith {
      case e: IllegalArgumentException => Task.now(println(e.getMessage)) *> getInitialGame
    }

  def runGameLoop(g: Game): Task[Unit] = runGameTurn(g).flatMap { next =>
    next.winner.fold(runGameLoop(next))(endGame)
  }

  def endGame(end: GameEndResult): Task[Unit] = Task {
    println("Game over!")
    println(end)
  }

  def runGameTurn(g: Game): Task[Game] =
    printGame(g) *> runActionLoop(g)

  def runActionLoop(g: Game): Task[Game] = runAction(g.nextPlayer).flatMap { action =>
    g.takeTurn(action).fold(Task.fail, Task.now)
  }.handleWith {
    case InvalidMove(reason) => Task.now(println(s"Bad move: $reason")) *> runActionLoop(g)
  }

  def runAction(nextPlayer: Player): Task[TurnAction] = promptAction.map(_(nextPlayer))

  def printGame(g: Game) = Task {
    val nextPlayInfo = g.turnNumber match {
      case 1 => "White to play (Black stone)"
      case 2 => "Black to play (White stone)"
      case n if n % 2 == 0 => "Black to play"
      case _ => "White to play"
    }
    println(s"Move ${g.turnNumber} - $nextPlayInfo")
    print(pretty(g))
    println()
  }

  def printStartup = Task {
    println("TakCLI")
  }

  def promptSize: Task[Int] =
    Task {
      print("Game size?\n  > ")
      StdIn.readInt()
    }.handleWith {
      case n: NumberFormatException => Task(println(s"Bad size: $n")) *> promptSize
    }

  def pretty(g: Game): String = {
    g.currentBoard.boardPositions.map(_.reverse).transpose.map(_.map(_.toTps).mkString("\t")).mkString("\n")
  }

  def promptAction: Task[Player => TurnAction] =
    Task(StdIn.readLine("Your Move?\n  > "))
      .flatMap { input =>
        if (input == null) { throw CleanExit } else
          PtnParser
            .parseEither(PtnParser.turnAction, input)
            .fold(
              err => Task.fail(PtnParseError(err)),
              toAction => Task.now(toAction)
            )
      }
      .handleWith {
        case PtnParseError(err) => Task(println(s"Bad move: $err")) *> promptAction
      }
}

case class PtnParseError(msg: String) extends Exception(msg) with NoStackTrace
case object CleanExit extends Exception with NoStackTrace
