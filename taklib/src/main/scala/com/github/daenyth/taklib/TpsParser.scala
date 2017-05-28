package com.github.daenyth.taklib

import com.github.daenyth.taklib.Implicits.RichParsing
import com.github.daenyth.taklib.Stone._

import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object TpsParser extends RegexParsers with RichParsing {
  override val skipWhitespace = false

  val tps: Parser[(Board, Int, Player)] = {
    val turn = """\d+""".r
    val nextPlayer = """1|2""".r
    val stack: Parser[Vector[Stack]] = "(1|2)+[SC]?".r ^^ { ss =>
      @tailrec
      def go(owners: List[Player], stack: List[Stone], finalStone: Player => Stone): List[Stone] =
        owners match {
          case Nil => Nil
          case owner :: Nil => finalStone(owner) :: Nil
          case owner :: os => go(os, FlatStone(owner) :: stack, finalStone)
        }

      val (drop, finalStone) =
        ss.last match {
          case 'S' => (1, StandingStone.apply _)
          case 'C' => (1, Capstone.apply _)
          case _ => (0, FlatStone.apply _)
        }
      val owners = ss.dropRight(drop).toList.map {
        case '1' => White
        case '2' => Black
      }
      Vector(Stack(go(owners, Nil, finalStone).toVector))
    }
    val empty = """x\d?""".r ^^ { xn =>
      val n = xn.substring(1)
      Vector.fill(Try(n.toInt).getOrElse(1))(Stack.empty)
    }
    val space: Parser[Vector[Stack]] = empty | stack
    val row: Parser[Vector[Stack]] = rep1sep(space, ",") ^^ { xs: List[Vector[Stack]] =>
      xs.toVector.flatten: Vector[Stack]
    }
    val board = rep1sep(row, "/")
    board ~ " " ~ nextPlayer ~ " " ~ turn ^^? {
      case bd ~ _ ~ np ~ _ ~ t =>
        val pieces: Vector[Vector[Stack]] = bd.toVector
        val ranksize = pieces.size

        if (! (for (file <- pieces) yield file.size).forall(_ == ranksize)) {
          scala.Left("Board size is not square")
        } else {

          val player = np match {
            case "1" => White
            case "2" => Black
          }
          scala.Right((Board(ranksize, pieces), t.toInt, player))
        }
    }
  }
}
