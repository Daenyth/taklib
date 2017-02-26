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
    for {
      _ <- printGame(g)
      toAction <- promptAction
      action = toAction(g.nextPlayer)
      next <- g.takeTurn(action).fold(Task.fail, Task.now)
    } yield next

  def printGame(g: Game) = Task {
    println(s"Move ${g.turnNumber}")
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

  def pretty(g: Game): String =
    g.toTps

  def promptAction: Task[Player => TurnAction] =
    Task(StdIn.readLine("Your Move?\n  > "))
      .flatMap { input =>
        if (input == null) { throw CleanExit } else
        PtnParser
          .parseEither(PtnParser.turnAction, input)
          .fold(
            err => Task.fail(new PtnParseError(err)),
            toAction => Task.now(toAction)
          )
      }
      .handleWith {
        case PtnParseError(err) => Task(println(s"Bad move: $err")) *> promptAction
      }
}

case class PtnParseError(msg: String) extends Exception(msg) with NoStackTrace
case object CleanExit extends Exception with NoStackTrace
