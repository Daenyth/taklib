package com.github.daenyth.taklib

import scala.annotation.tailrec
import scalaz.NonEmptyList
import scalaz.syntax.std.vector._
import scalaz.syntax.monoid._

object BoardState {

  /** Build a board from Tak Positional System; None if tps is invalid */
  def fromTPS(tps: String): Option[BoardState] = ???

  def empty(size: Int): BoardState =
    BoardState(size, Vector.fill(size, size)(EmptySpace))

  private def setStackAt(
      positions: Vector[Vector[BoardPosition]],
      index: BoardIndex,
      stack: BoardPosition): Vector[Vector[BoardPosition]] = {
    // TODO uses lenses instead of manual indexing/updating
    val (i, j) = (index.rank - 1, index.file - 1)
    positions.updated(i, positions(i).updated(j, stack))
  }

  private def combineStackAt(positions: Vector[Vector[BoardPosition]],
                             index: BoardIndex,
                             stack: Stack): Vector[Vector[BoardPosition]] = {
    val (i, j) = (index.rank - 1, index.file - 1)
    val currentStack = positions(i)(j)
    val newStack = currentStack match {
      case EmptySpace => stack
      // TODO need to check that top of target space isn't a noble; if it is, InvalidMove
      case Stack(pieces) => Stack(pieces |+| stack.pieces)
    }
    setStackAt(positions, index, newStack)
  }
}

case class BoardState(size: Int, boardPositions: Vector[Vector[BoardPosition]]) {
  import BoardState._

  // TODO fix type unsafety by switching to InvalidMove \/ BoardState
  // Will need to move the "is move valid" checks internal and switch to also be \/ for composing.
  def applyAction(action: TurnAction): BoardState = action match {
    case PlayStone(at, stone) =>
      val stack = Stack(NonEmptyList(stone))
      val newPositions = setStackAt(boardPositions, at, stack)
      BoardState(size, newPositions)
    case Move(player, from, direction, count, drops) =>
      @tailrec
      def spreadStack(movingStack: Vector[Stone],
                      index: BoardIndex,
                      drops: List[Int],
                      positions: Vector[Vector[BoardPosition]])
        : Vector[Vector[BoardPosition]] = {
        drops match {
          case Nil => positions
          case num :: ds =>
            val leaving = movingStack.take(num)
            val stillMoving = movingStack.drop(num)
            // .get smell, refactor to be type safe
            val newPositions =
              combineStackAt(positions, index, Stack(leaving.toNel.get))
            if (stillMoving.nonEmpty)
              spreadStack(stillMoving,
                          index.neighbor(direction),
                          ds,
                          newPositions)
            else newPositions
        }
      }

      val stack = positionAt(from).asInstanceOf[Stack]
      val (remainingStack, movingStack) =
        stack.pieces.list.splitAt(stack.size - count)
      assert(movingStack.length == count)

      val positionsWithoutMovedStones = setStackAt(
        boardPositions,
        from,
        remainingStack.toNel.map(Stack).getOrElse(EmptySpace))
      val newPositions = spreadStack(movingStack.toVector,
                                     from.neighbor(direction),
                                     drops.toList,
                                     positionsWithoutMovedStones)

      BoardState(size, newPositions)
  }

  def positionAt(index: BoardIndex): BoardPosition =
    boardPositions(index.rank - 1)(index.file - 1)

  def hasIndex(index: BoardIndex): Boolean =
    index.rank < size && index.rank >= 0 && index.file < size && index.file >= 0

  /** Serialize board state to Tak Positional System */
  def toTPS: String = ???
}

sealed trait BoardPosition // TODO replace family with Stack(Vector[Stone])
case object EmptySpace extends BoardPosition
case class Stack(pieces: NonEmptyList[Stone]) extends BoardPosition {
  def controller: Player = pieces.last.owner
  def size: Int = pieces.size
}

sealed trait Player
case object Black extends Player
case object White extends Player

sealed trait Stone {
  val owner: Player
}
case class Capstone(owner: Player) extends Stone
case class StandingStone(owner: Player) extends Stone
case class FlatStone(owner: Player) extends Stone
