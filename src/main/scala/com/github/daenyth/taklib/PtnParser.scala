package com.github.daenyth.taklib

import scala.util.parsing.combinator.RegexParsers

object PtnParser extends RegexParsers {
  val boardIndex: Parser[BoardIndex] = "([abcdefgh])([12345678])".r ^^ { str =>
    val rankChr = str.charAt(0)
    val rank = BoardIndex.rankNames.indexOf(rankChr) + 1
    val file = str.charAt(1).toInt
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
    val drops = "[12345678]+".r ^^ { _.toVector.map(_.toInt) }
    (count.? ~ boardIndex ~ moveDirection ~ drops.?) ^^ {
      case (count: Option[Int]) ~
            (idx: BoardIndex) ~
            (direction: MoveDirection) ~
            (drops: Option[Vector[Int]]) =>
        player =>
          Move(player, idx, direction, count, drops)
    }
  }
  val turnAction: Parser[Player => TurnAction] = playStone | moveStones
}
