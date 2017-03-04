package com.github.daenyth.taklib

import org.scalatest.{FlatSpec, Matchers}
import org.typelevel.scalatest.DisjunctionValues

import scala.io.Source

class PtnParserTest extends FlatSpec with Matchers with DisjunctionValues with MoveResultValues {
  def readPtn(name: String) = Source.fromResource(s"ptn/$name.ptn").getLines.mkString("\n")

  def parsePtn(ptn: String) = PtnParser.parseEither(PtnParser.ptn(DefaultRules), ptn)

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
    val tg: Game = Game.fromTps(tps).value
    val pg: Game = result.value
    tg.nextPlayer shouldEqual pg.nextPlayer
    tg.turnNumber shouldEqual pg.turnNumber
    tg.currentBoard shouldEqual pg.currentBoard
  }
}
