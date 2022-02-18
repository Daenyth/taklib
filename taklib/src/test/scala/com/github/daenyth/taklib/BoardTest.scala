package com.github.daenyth.taklib

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardTest extends AnyFlatSpec with Matchers {
  "An edge BoardIndex" should "be opposite one side" in {
    val idx = BoardIndex(1, 2)
    val opposites = idx.oppositeIndexes(5).toSet
    opposites shouldEqual Set(
      BoardIndex(5, 1),
      BoardIndex(5, 2),
      BoardIndex(5, 3),
      BoardIndex(5, 4),
      BoardIndex(5, 5)
    )
  }

  "A corner BoardIndex" should "be opposite two sides" in {
    val idx = BoardIndex(1, 1)
    val opposites = idx.oppositeIndexes(5).toSet
    opposites shouldEqual Set(
      BoardIndex(5, 1),
      BoardIndex(5, 2),
      BoardIndex(5, 3),
      BoardIndex(5, 4),
      BoardIndex(5, 5),
      BoardIndex(4, 5),
      BoardIndex(3, 5),
      BoardIndex(2, 5),
      BoardIndex(1, 5)
    )
  }

  "A board" should "have all indexes under its size" in {
    val size = 5
    val indexes = for {
      rank <- 1 to size
      file <- 1 to size
    } yield BoardIndex(rank, file)
    val board = Board.ofSize(size)
    indexes.forall(board.hasIndex) shouldBe true
  }
}
