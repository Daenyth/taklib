package com.github.daenyth.taklib

import org.scalatest.{FlatSpec, Matchers}
import org.typelevel.scalatest.DisjunctionValues

class MoveTest extends FlatSpec with Matchers with DisjunctionValues {

  behavior of "BoardState"

  it should "doMoveAction for simple capture" in {
    val board = BoardState.empty(5)
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

  it should "reject moving off the board" in {
    val board = BoardState.empty(5)
    val idx = BoardIndex(1, 1)
    val result = board.applyActions(
      PlayFlat(White, idx),
      Move(White, idx, Left, Some(1), Some(Vector(1)))
    )
    result shouldBe 'left
  }

}
