package com.github.daenyth.taklib

import com.github.daenyth.taklib.BoardState._

import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}
import scalaz.std.vector._
import scalaz.syntax.either._
import scalaz.syntax.monoid._

object BoardState {

  type Board = Vector[Vector[Stack]]
  type Checked[A] = InvalidMove.type \/ A

  /** Build a board from Tak Positional System; None if tps is invalid */
  def fromTPS(tps: String): Option[BoardState] = ???

  def empty(size: Int): BoardState =
    BoardState(size, Vector.fill(size, size)(Stack.empty))

  private def setStackAt(positions: Board, index: BoardIndex, stack: Stack): Board = {
    // TODO uses lenses instead of manual indexing/updating
    val (i, j) = (index.rank - 1, index.file - 1)
    positions.updated(i, positions(i).updated(j, stack))
  }

  private def combineStackAt(positions: Board, index: BoardIndex, stack: Stack): Checked[Board] = {
    val (i, j) = (index.rank - 1, index.file - 1)
    val stackAtIdx = \/.fromTryCatchNonFatal(positions(i)(j)).leftMap(_ => InvalidMove)
    val newStack: Checked[Stack] = stackAtIdx.flatMap {
      case Stack(Vector()) => stack.right
      case Stack(pieces) =>
        pieces.last match {
          case Capstone(_) => InvalidMove.left
          case FlatStone(_) => Stack(pieces |+| stack.pieces).right
          case StandingStone(owner) =>
            stack match {
              case Stack(Vector(c@Capstone(_))) =>
                Stack(pieces.init |+| Vector(FlatStone(owner), c)).right
              case _ => InvalidMove.left
            }
        }
    }
    newStack.map(setStackAt(positions, index, _))
  }
}

case class BoardState(size: Int, boardPositions: Board) {

  def applyAction(action: TurnAction): Checked[BoardState] = action match {
    case PlayStone(at, stone) =>
      val stack = Stack.of(stone)
      val newPositions = setStackAt(boardPositions, at, stack)
      BoardState(size, newPositions).right
    case m: Move => doMoveAction(m)
  }

  // TODO test this
  private[taklib] def doMoveAction(m: Move): InvalidMove.type \/ BoardState = {
    @tailrec
    def spreadStack(movingStack: Vector[Stone],
                    index: BoardIndex,
                    drops: List[Int],
                    positions: Board): Checked[Board] =
      drops match {
        case Nil => positions.right
        case num :: ds =>
          val (leaving, stillMoving) = movingStack.splitAt(num)
          val combined = combineStackAt(positions, index, Stack(leaving))

          // This match is flatMap, but inlined so that scala can see that it's tailrec
          combined match {
            case \/-(newPositions) =>
              if (stillMoving.nonEmpty)
                spreadStack(stillMoving, index.neighbor(m.direction), ds, newPositions)
              else newPositions.right
            case e @ -\/(_) => e
          }
      }

    val stack = stackAt(m.from)
    val (remainingStack, movingStack) =
      stack.pieces.splitAt(stack.size - m.count)
    assert(movingStack.length == m.count, s"moving: $movingStack, remaining: $remainingStack")

    val positionsWithoutMovedStones =
      setStackAt(boardPositions, m.from, Stack(remainingStack))
    val finalPositions =
      spreadStack(
        movingStack,
        m.from.neighbor(m.direction),
        m.drops.toList,
        positionsWithoutMovedStones
      )

    finalPositions.map(BoardState(size, _))
  }

  def stackAt(index: BoardIndex): Stack =
    boardPositions(index.rank - 1)(index.file - 1)

  def hasIndex(index: BoardIndex): Boolean =
    index.rank < size && index.rank >= 0 && index.file < size && index.file >= 0

  /** Serialize board state to Tak Positional System */
  def toTPS: String = ???
}

object Stack {
  val empty = Stack(Vector.empty[Stone])
  def of(s: Stone) = Stack(Vector(s))
}
case class Stack(pieces: Vector[Stone]) {
  def controller: Player = pieces.last.owner
  def size: Int = pieces.size
  def isEmpty: Boolean = pieces.isEmpty
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
