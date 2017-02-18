package com.github.daenyth.taklib

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scalaz.syntax.semigroup._

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

class GameTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import GameTest._

  "GameEndResult" should "be a lawful semigroup" in {
    forAll { (g1: GameEndResult, g2: GameEndResult, g3: GameEndResult) =>
      (g1 |+| (g2 |+| g3)) shouldEqual ((g1 |+| g2) |+| g3)
    }
  }

  "A full board" should "have a game end result" in {
    val board = BoardState.fromTPS("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 1 1 ]").get
    val game = Game(board)
    game.flatWin shouldBe 'some
  }
}
