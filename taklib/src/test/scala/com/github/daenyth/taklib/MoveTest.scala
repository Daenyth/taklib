package com.github.daenyth.taklib

import org.scalatest.{FlatSpec, Matchers}

class MoveTest extends FlatSpec with Matchers with MoveResultValues {

  "Moving to capture" should "make a stack" in {
    val board = Board.ofSize(5)
    val idx = BoardIndex(1, 1)
    val neighbor = idx.neighbor(Right)
    val finalBoard = board.applyActions(
      PlayFlat(White, idx),
      PlayFlat(Black, neighbor),
      Move(White, idx, Right, None, None)
    )
    val result = for {
      b <- finalBoard
      a1 <- b.stackAt(idx)
      a2 <- b.stackAt(neighbor)
    } yield (a1, a2)
    val (a1, a2) = result.value
    a1 shouldBe Stack.empty
    a2 shouldBe Stack(Vector(FlatStone(Black), FlatStone(White)))
  }

  "Moving off the board" should "be rejected" in {
    val board = Board.ofSize(5)
    val idx = BoardIndex(1, 1)
    val result = board.applyActions(
      PlayFlat(White, idx),
      Move(White, idx, Left, Some(1), Some(Vector(1)))
    )
    result shouldBe an[InvalidMove]
  }

  "A capstone" should "flatten standing stones by itself" in {
    val i = BoardIndex(1, 1)
    val j = i.neighbor(Right)
    val board = Board
      .ofSize(5)
      .applyActions(PlayStanding(White, i), PlayCapstone(Black, j))
      .value
    val result = for {
      afterMove <- board.applyAction(Move(Black, j, Left, None, None))
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
        PlayStanding(White, i),
        PlayFlat(White, j),
        PlayCapstone(Black, k),
        Move(Black, k, Left, None, None)
      )
      .value
    val result = board.applyAction(Move(Black, j, Left, None, None))
    result shouldBe an[InvalidMove]
  }

  "An existing stack" should "prevent a new stack from being played at that space" in {
    val idx = BoardIndex(1, 1)
    val board = Board.ofSize(5).applyAction(PlayFlat(White, idx)).value
    board.applyAction(PlayFlat(White, idx)) shouldBe an[InvalidMove]
  }
}
