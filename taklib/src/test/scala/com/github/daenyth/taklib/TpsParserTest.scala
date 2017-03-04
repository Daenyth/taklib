package com.github.daenyth.taklib

import org.scalatest.{FlatSpec, Matchers}
import org.typelevel.scalatest.DisjunctionValues

class TpsParserTest extends FlatSpec with Matchers with DisjunctionValues {
  "A size 6 board empty board" should "be parsed" in {
    Board.fromTps("x6/x6/x6/x6/x6/x6 1 1").value shouldEqual Board.ofSize(6)
  }

  "A board with a stack" should "be parsed" in {
    Board.fromTps("x3/x3/121,x2 2 1") shouldBe 'right
  }
}
