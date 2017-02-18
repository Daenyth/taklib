package com.github.daenyth.taklib

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers}

import scalaz.scalacheck.ScalazProperties

object GameTest {
  implicit val arbRoad: Arbitrary[RoadWin] = Arbitrary { Gen.oneOf(White, Black).map(RoadWin) }
  implicit val arbFlat: Arbitrary[FlatWin] = Arbitrary { Gen.oneOf(White, Black).map(FlatWin) }
  implicit val arbGer: Arbitrary[GameEndResult] = Arbitrary {
    Gen.oneOf(
      Gen.const(DoubleRoad),
      Gen.const(Draw),
      arbRoad.arbitrary,
      arbFlat.arbitrary
    )
  }
}

class GameTest extends FlatSpec with Matchers with PropertyCheckers {
  import GameTest._

  "GameEndResult" should "be a lawful semigroup" in {
    check(ScalazProperties.semigroup.laws[GameEndResult])
  }

  "A full board" should "have a game end result" in {
    val board = BoardState.fromTPS("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 1 1 ]").get
    val game = Game(board)
    game.flatWin shouldBe 'some
  }
}
