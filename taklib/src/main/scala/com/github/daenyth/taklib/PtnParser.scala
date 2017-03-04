package com.github.daenyth.taklib

import scala.collection.immutable.VectorBuilder
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers
import scalaz.\/
import scalaz.std.vector._
import scalaz.syntax.foldable._

object PtnParser extends RegexParsers {
  val boardIndex: Parser[BoardIndex] = "([abcdefgh])([12345678])".r ^^ { str =>
    val rankChr = str.charAt(0)
    val rank = BoardIndex.rankNames.indexOf(rankChr) + 1
    val file = str.charAt(1).toString.toInt
    BoardIndex(rank, file)
  }
  val playFlat: Parser[Player => PlayFlat] =
    boardIndex ^^ { idx => player =>
      PlayFlat(player, idx)
    }
  val playStanding: Parser[Player => PlayStanding] =
    "S".r ~ boardIndex ^^ {
      case (_ ~ idx) =>
        player =>
          PlayStanding(player, idx)
    }
  val playCapstone: Parser[Player => PlayCapstone] =
    "C".r ~ boardIndex ^^ {
      case (_ ~ idx) =>
        player =>
          PlayCapstone(player, idx)
    }
  val playStone: Parser[Player => PlayStone] =
    playFlat | playStanding | playCapstone

  val moveDirection: Parser[MoveDirection] = "[-+<>]".r ^^ {
    case "-" => Down
    case "+" => Up
    case "<" => Left
    case ">" => Right
  }

  val moveStones: Parser[Player => Move] = {
    val count = "[12345678]".r ^^ { _.toInt }
    val drops = "[12345678]+".r ^^ { _.toVector.map(_.toString.toInt) }
    (count.? ~ boardIndex ~ moveDirection ~ drops.?) ^^ {
      case (count: Option[Int]) ~
            (idx: BoardIndex) ~
            (direction: MoveDirection) ~
            (drops: Option[Vector[Int]]) =>
        player =>
          Move(player, idx, direction, count, drops)
    }
  }
  val turnAction: Parser[Player => TurnAction] = moveStones | playStone

  val headerLine: Parser[(String, String)] = "[" ~ """\S+""".r ~ "\".*\"".r ~ "]" ^^ {
    case _ ~ headerKey ~ headerValue ~ _ =>
      (headerKey, headerValue.substring(1, headerValue.length - 1))
  }

  // TODO expose a well-typed PtnHeader class with known keys as fields
  val headers: Parser[Map[String, String]] = rep(headerLine).map(_.toMap)

  val fullTurnLine: Parser[(Int, Player => TurnAction, Player => TurnAction)] = """\d+\.""".r ~ turnAction ~ turnAction ^^ {
    case turnNumber ~ whiteAction ~ blackAction =>
      (turnNumber.dropRight(1).toInt, whiteAction, blackAction)
  }

  val lastTurnLine: Parser[(Int, Player => TurnAction, Option[Player => TurnAction])] = """\d+\.""".r ~ turnAction ~ turnAction.? ^^ {
    case turnNumber ~ whiteAction ~ blackAction =>
      (turnNumber.dropRight(1).toInt, whiteAction, blackAction)
  }

  // TODO parse TPS as starting board
  val gameHistory: Parser[Vector[TurnAction]] = rep(fullTurnLine) ~ lastTurnLine.? ^^ {
    case fullturns ~ lastTurn =>
      var nextTurnNumber = 1
      val iter = fullturns.iterator
      val history = new VectorBuilder[TurnAction]
      while (iter.hasNext) {
        val (turnNumber, whiteAction, blackAction) = iter.next()
        val stoneColor = if (turnNumber == 1) (Black, White) else (White, Black)
        assert(
          turnNumber == nextTurnNumber,
          s"Turn numbers out of order; expected $nextTurnNumber, got $turnNumber"
        )
        nextTurnNumber += 1
        history += whiteAction(stoneColor._1)
        history += blackAction(stoneColor._2)
      }
      lastTurn.foreach {
        case (turnNumber, whiteAction, blackAction) =>
          val stoneColor = if (turnNumber == 1) (Black, White) else (White, Black)
          assert(
            turnNumber == nextTurnNumber,
            s"Turn numbers out of order; expected $nextTurnNumber, got $turnNumber"
          )
          history += whiteAction(stoneColor._1)
          blackAction.foreach(a => history += a(stoneColor._2))
      }
      history.result()
  }

  val infoMark: Parser[String] = "'{1,2}".r | "[!?]{1,2}".r

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

  def ptn(ruleSet: RuleSet): Parser[MoveResult[Game]] = headers ~ gameHistory ^^ {
    case gameHeaders ~ history =>
      val size = Try(gameHeaders("Size").toInt)
        .getOrElse(throw new Exception("Unable to parse game size from header"))
      val initialGame = Game.ofSize(size, ruleSet).fold(err => throw new Exception(err), identity)
      history.zipWithIndex.foldLeftM[MoveResult, Game](initialGame) { case (game, (action, actionIdx)) =>
          game.takeTurn(action).noteInvalid(r => s"(Move #$actionIdx) $r")
      }
  }
  def parseEither[T](parser: PtnParser.Parser[T], ptn: String): String \/ T =
    parse(parser, ptn) match {
      case Success(result, _) => \/.right(result)
      case err: NoSuccess => \/.left(err.msg)
    }
}
