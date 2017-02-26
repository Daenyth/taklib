package com.github.daenyth.taklib

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers, OptionValues}
import org.typelevel.scalatest.DisjunctionValues

import scalaz.\/
import scalaz.scalacheck.ScalazProperties
import scalaz.syntax.traverse._
import scalaz.std.vector._

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

  "A TPS string" should "round trip through Game" in {
    val tps = "[ 1,2,1,2,1/2,1,2,1,2/1,2,1,2,1/2,1,2,1,2/1,2,1,2,1 12 2 ]"
    val game = Game.fromTps(tps).value
    game.toTps shouldEqual tps
  }

  "A game's tps" should "round trip to the same game" in {
    val game1 = (for {
      a <- Game.ofSize(5).takeTurn(PlayFlat(Black, BoardIndex(1, 1)))
      b <- a.takeTurn(PlayFlat(White, BoardIndex(5, 1)))
    } yield b).value
    val tps = game1.toTps
    val game2 = Game.fromTps(tps).value
    game1.size shouldEqual game2.size
    game1.turnNumber shouldEqual game2.turnNumber
    game1.currentBoard shouldEqual game2.currentBoard
  }

  "A long game" should "be playable without a problem" in {
    // https://www.playtak.com/games/153358/view
    val maybeMoves: Vector[String \/ (Player => TurnAction)] = Vector(
      "a6",
      "a1",
      "c3",
      "c4",
      "d3",
      "d4",
      "e4",
      "e3",
      "f4",
      "b4",
      "b3",
      "Cd2",
      "1d3+1",
      "1d2+1",
      "2d4<2",
      "1d3<1",
      "3c4<3",
      "2c3<2",
      "Cc4",
      "2b3+2",
      "a3",
      "b2",
      "d4",
      "c3",
      "c2",
      "1c3<1",
      "c3",
      "b1",
      "1c2<1",
      "1b1+1",
      "b1",
      "a5",
      "1a3>1",
      "2b2+2",
      "Sb5",
      "b6",
      "c2",
      "c1",
      "1c2<1",
      "3b3-3",
      "Sc2",
      "5b4-14",
      "1c4<1",
      "a4",
      "a3",
      "3b3<3",
      "Sa2",
      "4b2+4",
      "1c2<1",
      "d3",
      "4b2>112",
      "1c1+1",
      "1a2+1",
      "4b3>13",
      "c4",
      "3d3+3",
      "5a3>113",
      "4d4<4",
      "2c3-2",
      "Sa2",
      "d4",
      "1a2>1",
      "3c2-3",
      "2b2>2",
      "d5",
      "3c2-3",
      "d1",
      "c2",
      "d6",
      "a2",
      "2b4-2",
      "5c4-5",
      "c4",
      "a3",
      "1b5<1",
      "b4",
      "3b3+3",
      "1e3+1",
      "e5",
      "f5",
      "1e5-1",
      "6c3+6",
      "c5",
      "Se5",
      "b5",
      "1e5-1",
      "e6",
      "f6",
      "b3",
      "c6",
      "e5",
      "4e4>4",
      "e4",
      "5c1>14",
      "e3"
    ).map { PtnParser.parseEither(PtnParser.turnAction, _) }
    val toActions: Vector[Player => TurnAction] = maybeMoves.sequenceU.value
    val actions = toActions.zipWithIndex.map {
      case (action, 0) => action(Black)
      case (action, 1) => action(White)
      case (action, n) if n % 2 == 0 => action(White)
      case (action, _) => action(Black)
    }
    val game = actions.foldlM(Game.ofSize(6)) { game => action => game.takeTurn(action) }
    game.value.winner.value shouldEqual FlatWin(White)
  }
}
