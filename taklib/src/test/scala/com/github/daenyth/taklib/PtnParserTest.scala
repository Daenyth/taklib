package com.github.daenyth.taklib

import com.github.daenyth.taklib.GameEndResult._
import com.github.daenyth.taklib.PtnParser.PtnHeaders
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

object PtnParserTest {
  def readPtn(ptnFileName: String): String =
    Source.fromResource(s"ptn/$ptnFileName.ptn").getLines().mkString("\n")

  def parsePtn(ptn: String): Either[String, (PtnHeaders, MoveResult[Game])] =
    PtnParser.parseEither(PtnParser.ptn(DefaultRules), ptn)

}

class PtnParserTest extends AnyFlatSpec with Matchers with EitherValues with MoveResultValues {
  import PtnParserTest._

  "BoardIndex names" should "round trip ptn parsing" in {
    val size = 5
    val indexes = for {
      rank <- 1 to size
      file <- 1 to size
    } yield BoardIndex(rank, file)
    val parsed = indexes.map(i => PtnParser.parseEither(PtnParser.boardIndex, i.name))
    parsed.zip(indexes).foreach { case (result, idx) => result.value shouldEqual idx }
  }

  "PTN files" should "parse with no errors" in {
    val ptn = readPtn("game1")
    val result: MoveResult[Game] = parsePtn(ptn).value._2
    result should not be an[InvalidMove]
  }

  "PTN files with TPS tags" should "parse with a built board" in {
    val ptn = readPtn("game2")
    val (headers, result: MoveResult[Game]) = parsePtn(ptn).value
    val tps = headers("TPS")
    val gameFromTps: Game = Game.fromTps(tps).value
    val gameFromPtn: Game = result.value
    gameFromTps.nextPlayer shouldEqual gameFromPtn.nextPlayer
    gameFromTps.turnNumber shouldEqual gameFromPtn.turnNumber
    gameFromTps.currentBoard shouldEqual gameFromPtn.currentBoard
  }

  "An 8x8 game" should "work" in {
    val ptn = readPtn("8x8")
    val (_, result: MoveResult[Game]) = parsePtn(ptn).value
    result should matchPattern { case GameOver(RoadWin(White), _) =>
      ()
    }
  }

  "A game with annotation comments" should "parse correctly" in {
    val ptn = readPtn("annot1")
    val (_, result) = parsePtn(ptn).value
    result should matchPattern { case GameOver(RoadWin(White), _) =>
      ()
    }
  }
}
