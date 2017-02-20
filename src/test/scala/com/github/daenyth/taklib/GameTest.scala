package com.github.daenyth.taklib

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers, OptionValues}
import org.typelevel.scalatest.DisjunctionValues

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

class GameTest extends FlatSpec with Matchers with PropertyCheckers with OptionValues with DisjunctionValues {
  import GameTest._

  "GameEndResult" should "be a lawful semigroup" in {
    check(ScalazProperties.semigroup.laws[GameEndResult])
  }

  "A full board" should "have a game end result" in {
    val board = BoardState.fromTPS("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 1 1 ]").get
    val game = Game.fromBoard(board)
    game.winner.value shouldEqual FlatWin(White)
  }

  "A full board with even flat count" should "be a draw" in {
    val board = BoardState.fromTPS("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1S 1 1 ]").get
    val game = Game.fromBoard(board)
    game.winner.value shouldEqual Draw
  }

  "A new game" should "not have a winner" in {
    val game = Game.ofSize(5)
    game.winner shouldBe None
  }

  "A board with 5 stones in a row" should "have a road win" in {
    val board = BoardState.empty(5)
    val roadBoard = board.applyActions((1 to 5).map(n => PlayFlat(White, BoardIndex(1, n))))
    val game = Game.fromBoard(roadBoard.value)
    game.winner.value shouldBe RoadWin(White)
  }
}
