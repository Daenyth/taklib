package com.github.daenyth.taklib

import org.scalatest.{FlatSpec, Matchers}
import org.typelevel.scalatest.DisjunctionValues

class PtnParserTest extends FlatSpec with Matchers with DisjunctionValues {
  "BoardIndex names" should "round trip ptn parsing" in {
    val size = 5
    val indexes = for {
      rank <- 1 to size
      file <- 1 to size
    } yield BoardIndex(rank, file)
    val parsed = indexes.map(i => PtnParser.parseEither(PtnParser.boardIndex, i.name))
    parsed.zip(indexes).foreach { case (result, idx) => result.value shouldEqual idx }
  }
}
