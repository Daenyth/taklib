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

class GameTest
    extends FlatSpec
    with Matchers
    with PropertyCheckers
    with OptionValues
    with DisjunctionValues {
  import GameTest._

  "GameEndResult" should "be a lawful semigroup" in {
    check(ScalazProperties.semigroup.laws[GameEndResult])
  }

  "A full board" should "have a game end result" in {
    val game = Game.fromTps("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 12 2 ]").value
    game.winner.value shouldEqual FlatWin(White)
  }

  "A full board with even flat count" should "be a draw" in {
    val game = Game.fromTps("[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1S 1 1 ]").value
    game.winner.value shouldEqual Draw
  }

  "A new game" should "not have a winner" in {
    val game = Game.ofSize(5)
    game.winner shouldBe None
  }

  "A board with 5 stones in a row" should "have a road win" in {
    val board = Board.ofSize(5)
    val roadBoard = board.applyActions((1 to 5).map(n => PlayFlat(White, BoardIndex(1, n))))
    val game = Game.fromBoard(roadBoard.value)
    game.winner.value shouldBe RoadWin(White)
  }

  "Four flats and a capstone" should "have a road win" in {
    val board = Board.ofSize(5)
    val moves = (1 to 4).map(n => PlayFlat(Black, BoardIndex(1, n))) ++ Vector(
        PlayCapstone(Black, BoardIndex(1, 5))
      )
    val roadBoard = board.applyActions(moves)
    val game = Game.fromBoard(roadBoard.value)
    game.winner.value shouldBe RoadWin(Black)
  }

  "Four flats and a standing stone" should "not be a win" in {
    val board = Board.ofSize(5)
    val moves = (1 to 4).map(n => PlayFlat(Black, BoardIndex(1, n))) ++ Vector(
      PlayStanding(Black, BoardIndex(1, 5))
    )
    val roadBoard = board.applyActions(moves)
    val game = Game.fromBoard(roadBoard.value)
    game.winner shouldBe None
  }

  "A player" should "be able to move a stack they control" in {
    val i = BoardIndex(1, 1)
    val board = Board.ofSize(5).applyAction(PlayFlat(White, i)).value
    Game.actingPlayerControlsStack(board, Move(White, i, Right, None, None)) shouldBe 'right
  }

  "A player" should "not be able to move a stack they don't control" in {
    val i = BoardIndex(1, 1)
    val board = Board.ofSize(5).applyAction(PlayFlat(Black, i)).value
    Game.actingPlayerControlsStack(board, Move(White, i, Right, None, None)) shouldBe 'left
  }

  "The first move" should "be taken with a black flatstone" in {
    val game = Game.ofSize(5)
    val result = game.takeTurn(PlayFlat(Black, BoardIndex(1, 1)))
    result shouldBe 'right
  }
}
