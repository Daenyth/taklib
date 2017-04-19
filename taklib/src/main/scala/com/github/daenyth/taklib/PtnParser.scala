package com.github.daenyth.taklib

import com.github.daenyth.taklib.GameEndResult._
import com.github.daenyth.taklib.Implicits.RichParsing

import scala.collection.immutable.VectorBuilder
import scala.util.parsing.combinator.RegexParsers
import scalaz.std.vector._
import scalaz.syntax.foldable._
import scalaz.{-\/, \/, \/-}

object PtnParser extends RegexParsers with RichParsing {

  // TODO expose a well-typed PtnHeader class with known keys as fields
  type PtnHeaders = Map[String, String]

  val boardIndex: Parser[BoardIndex] = "([abcdefgh])([12345678])".r ^^ { str =>
    val rankChr = str.charAt(0)
    val rank = BoardIndex.rankNames.indexOf(rankChr) + 1
    val file = str.charAt(1).toString.toInt
    BoardIndex(rank, file)
  }
  val playFlat: Parser[PlayFlat] = boardIndex ^^ { idx => PlayFlat(idx) }
  val playStanding: Parser[PlayStanding] = "S".r ~ boardIndex ^^ { case _ ~ idx => PlayStanding(idx) }
  val playCapstone: Parser[PlayCapstone] = "C".r ~ boardIndex ^^ { case _ ~ idx => PlayCapstone(idx) }
  val playStone: Parser[PlayStone] = playFlat | playStanding | playCapstone

  val moveDirection: Parser[MoveDirection] = "[-+<>]".r ^^ {
    case "-" => Down
    case "+" => Up
    case "<" => Left
    case ">" => Right
  }

  val moveStones: Parser[Move] = {
    val count = "[12345678]".r ^^ { _.toInt }
    val drops = "[12345678]+".r ^^ { _.toVector.map(_.toString.toInt) }
    (count.? ~ boardIndex ~ moveDirection ~ drops.?) ^^ {
      case (count: Option[Int]) ~
            (idx: BoardIndex) ~
            (direction: MoveDirection) ~
            (drops: Option[Vector[Int]]) =>
          Move(idx, direction, count, drops)
    }
  }

  val infoMark: Parser[String] = "'{1,2}".r | "[!?]{1,2}".r

  val turnAction: Parser[TurnAction] = (moveStones | playStone) ~ infoMark.? ^^ {
    case action ~ _ => action
  }

  val headerLine: Parser[(String, String)] = "[" ~ """\S+""".r ~ "\".*\"".r ~ "]" ^^ {
    case _ ~ headerKey ~ headerValue ~ _ =>
      (headerKey, headerValue.substring(1, headerValue.length - 1))
  }

  val headers: Parser[PtnHeaders] = rep(headerLine).map(_.toMap)

  val comment: Parser[String] = """(?s)\{(.*?)\}""".r

  val fullTurnLine: Parser[(Int, TurnAction, TurnAction)] =
    """\d+\.""".r ~ turnAction ~ turnAction ~ comment.? ^^ {
      case turnNumber ~ whiteAction ~ blackAction ~ _comment =>
        (turnNumber.dropRight(1).toInt, whiteAction, blackAction)
    }

  val lastTurnLine: Parser[(Int, TurnAction, Option[TurnAction])] =
    """\d+\.""".r ~ turnAction ~ turnAction.? ~ comment.? ^^ {
      case turnNumber ~ whiteAction ~ blackAction ~ _comment =>
        (turnNumber.dropRight(1).toInt, whiteAction, blackAction)
    }

  def gameHistory(startingTurn: Int, skipFirst: Boolean): Parser[Vector[TurnAction]] =
    rep(fullTurnLine) ~ lastTurnLine.? ^^? {
      case fullturns ~ lastTurn =>
        var nextTurnNumber = startingTurn
        val iter = fullturns.iterator
        val history = new VectorBuilder[TurnAction]
        \/.fromTryCatchNonFatal {
          while (iter.hasNext) {
            val (turnNumber, whiteAction, blackAction) = iter.next()
            require(
              turnNumber == nextTurnNumber,
              s"Turn numbers out of order; expected $nextTurnNumber, got $turnNumber"
            )
            nextTurnNumber += 1
            if (skipFirst && startingTurn == turnNumber) {
              // do nothing
            } else {
              history += whiteAction
            }
            history += blackAction
          }
          lastTurn.foreach {
            case (turnNumber, whiteAction, blackAction) =>
              require(
                turnNumber == nextTurnNumber,
                s"Turn numbers out of order; expected $nextTurnNumber, got $turnNumber"
              )
              if (skipFirst && startingTurn == turnNumber) {
                // do nothing
              } else {
                history += whiteAction
              }
              blackAction.foreach(history += _)
          }
          history.result()
        }.leftMap(_.getMessage)
    }

  val roadWin: Parser[RoadWin] = "R-0" ^^ { _ =>
    RoadWin(White)
  } | "0-R" ^^ { _ =>
    RoadWin(Black)
  }
  val flatWin: Parser[FlatWin] = "F-0" ^^ { _ =>
    FlatWin(White)
  } | "0-R" ^^ { _ =>
    FlatWin(Black)
  }
  val draw: Parser[Draw.type] = "1/2-1/2" ^^ { _ =>
    Draw
  }
  val resignation: Parser[WinByResignation] = "1-0" ^^ { _ =>
    WinByResignation(White)
  } | "0-1" ^^ { _ =>
    WinByResignation(Black)
  }
  val gameEnd: Parser[GameEndResult] = roadWin | flatWin | resignation | draw

  def ptn(ruleSet: RuleSet): Parser[(PtnHeaders, MoveResult[Game])] = headers >>=? { gameHeaders =>
    gameHeaders.get("TPS") match {
      case None =>
        \/-(gameHistoryFromTurn(ruleSet, gameHeaders, 1, skipFirst = false, Game.ofSize))
      case Some(tps) =>
        TpsParser.parseEither(TpsParser.tps, tps).flatMap {
          case (board, turnNumber, nextPlayer) =>
            def getGame(size: Int, ruleSet: RuleSet): String \/ Game = {
              val gameTurnNumber = (2 * turnNumber) + nextPlayer.fold(1, 0)
              val game = Game.fromBoard(board, gameTurnNumber)
              if (game.size != size)
                -\/(s"Game headers declared size $size but TPS contained size ${game.size}")
              else \/-(game)
            }

            \/-(
              gameHistoryFromTurn(
                ruleSet,
                gameHeaders,
                turnNumber,
                skipFirst = nextPlayer == Black,
                getGame
              )
            )
        }
    }
  }

  private def gameHistoryFromTurn(
      ruleSet: RuleSet,
      gameHeaders: PtnHeaders,
      startingTurn: Int,
      skipFirst: Boolean,
      getInitialGame: (Int, RuleSet) => String \/ Game
  ): Parser[(PtnHeaders, MoveResult[Game])] =
    gameHistory(startingTurn, skipFirst) ^^? { history =>
      for {
        size <- \/.fromTryCatchNonFatal(gameHeaders("Size").toInt)
          .leftMap(ex => s"Unable to parse game size from header ${ex.getMessage}")
        initialGame <- getInitialGame(size, ruleSet)
      } yield {
        val finalGame = history.zipWithIndex
          .foldLeftM[MoveResult, Game](initialGame) {
            case (game, (action, actionIdx)) =>
              game.takeTurn(action).noteInvalid(r => s"(Move #${actionIdx + 1}) $r")
          }
        (gameHeaders, finalGame)
      }
    }
}
