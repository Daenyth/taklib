package com.github.daenyth.taklib

import org.scalatest.{FlatSpec, Matchers}
import org.typelevel.scalatest.DisjunctionValues

class BoardStateTest extends FlatSpec with Matchers with DisjunctionValues {

  behavior of "BoardState"

  it should "doMoveAction for simple capture" in {
    val board = BoardState.empty(5)
    val idx = BoardIndex(1, 1)
    val neighbor = idx.neighbor(Right)
    val result = for {
      a <- board.applyAction(PlayFlat(White, idx))
      b <- a.applyAction(PlayFlat(Black, neighbor))
      c <- b.applyAction(Move(White, idx, Right, 1, Vector(1)))
    } yield (c.stackAt(idx), c.stackAt(neighbor))
    result.value shouldBe ((Stack.empty, Stack(Vector(FlatStone(Black), FlatStone(White)))))
  }

  it should "reject moving off the board" in {
    val board = BoardState.empty(5)
    val idx = BoardIndex(1, 1)
    val result = for {
      a <- board.applyAction(PlayFlat(White, idx))
      b <- a.applyAction(Move(White, idx, Left, 1, Vector(1)))
    } yield b
    result shouldBe 'left
  }

}
