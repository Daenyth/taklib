package com.github.daenyth.taklib

import com.github.daenyth.taklib.Stone._
import org.scalatest.{FlatSpec, Matchers}

class MoveTest extends FlatSpec with Matchers with MoveResultValues {

  "Moving to capture" should "make a stack" in {
    val board = Board.ofSize(5)
    val idx = BoardIndex(1, 1)
    val neighbor = idx.neighbor(Right)
    val result = for {
      a <- board.applyAction(White, PlayFlat(idx))
      b <- a.applyAction(Black, PlayFlat(neighbor))
      finalBoard <- b.applyAction(White, Move(idx, Right, None, None))
      a1 <- finalBoard.stackAt(idx)
      a2 <- finalBoard.stackAt(neighbor)
    } yield (a1, a2)
    val (a1, a2) = result.value
    a1 shouldBe Stack.empty
    a2 shouldBe Stack(Vector(FlatStone(Black), FlatStone(White)))
  }

  "Moving off the board" should "be rejected" in {
    val board = Board.ofSize(5)
    val idx = BoardIndex(1, 1)
    val result = board.applyActions(
      White -> PlayFlat(idx),
      White -> Move(idx, Left, Some(1), Some(Vector(1)))
    )
    result shouldBe an[InvalidMove]
  }

  "A capstone" should "flatten standing stones by itself" in {
    val i = BoardIndex(1, 1)
    val j = i.neighbor(Right)
    val board = Board
      .ofSize(5)
      .applyActions(White -> PlayStanding(i), Black -> PlayCapstone(j))
      .value
    val result = for {
      afterMove <- board.applyAction(Black, Move(j, Left, None, None))
      stack <- afterMove.stackAt(i)
    } yield stack
    result.value shouldEqual Stack(Vector(FlatStone(White), Capstone(Black)))
  }

  "A capstone" should "not be allowed to put a prisoner on top of a standing stone" in {
    val i = BoardIndex(1, 1)
    val j = i.neighbor(Right)
    val k = j.neighbor(Right)
    val board = Board
      .ofSize(5)
      .applyActions(
        White -> PlayStanding(i),
        White -> PlayFlat(j),
        Black -> PlayCapstone(k),
        Black -> Move(k, Left, None, None)
      )
      .value
    val result = board.applyAction(Black, Move(j, Left, None, None))
    result shouldBe an[InvalidMove]
  }

  "An existing stack" should "prevent a new stack from being played at that space" in {
    val idx = BoardIndex(1, 1)
    val board = Board.ofSize(5).applyAction(White, PlayFlat(idx)).value
    board.applyAction(White, PlayFlat(idx)) shouldBe an[InvalidMove]
  }
}
