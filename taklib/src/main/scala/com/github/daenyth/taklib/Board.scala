package com.github.daenyth.taklib

import com.github.daenyth.taklib.Board._
import com.github.daenyth.taklib.BooleanOps._

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq
import scalaz.std.vector._
import scalaz.syntax.either._
import scalaz.syntax.monoid._
import scalaz.syntax.std.option._
import scalaz.{-\/, \/, \/-, Equal}

object Board {

  type BoardLayout = Vector[Vector[Stack]]
  type Checked[A] = InvalidMove \/ A

  /** Build a board from Tak Positional System; -\/ if tps is invalid */
  def fromTps(tps: String): String \/ Board = TpsParser.parse(TpsParser.board, tps) match {
    case TpsParser.Success((board, _, _), _) => board.right
    case err: TpsParser.NoSuccess => err.msg.left
  }

  def ofSize(size: Int): Board =
    Board(size, Vector.fill(size, size)(Stack.empty))

  private def setStackAt(positions: BoardLayout, index: BoardIndex, stack: Stack): BoardLayout = {
    // TODO uses lenses instead of manual indexing/updating
    val (i, j) = (index.file - 1, index.rank - 1)
    positions.updated(i, positions(i).updated(j, stack))
  }

  private def combineStackAt(positions: BoardLayout,
                             index: BoardIndex,
                             stack: Stack): Checked[BoardLayout] = {
    val (i, j) = (index.file - 1, index.rank - 1)
    val stackAtIdx = \/.fromTryCatchNonFatal(positions(i)(j))
      .leftMap(_ => InvalidMove(s"$index is not on the board"))
    val newStack: Checked[Stack] = stackAtIdx.flatMap {
      case Stack(Vector()) => stack.right
      case Stack(pieces) =>
        pieces.last match {
          case Capstone(_) => InvalidMove(s"Cannot move on top of Capstone at $index").left
          case FlatStone(_) => Stack(pieces |+| stack.pieces).right
          case StandingStone(owner) =>
            stack match {
              case Stack(Vector(c @ Capstone(_))) =>
                Stack(pieces.init |+| Vector(FlatStone(owner), c)).right
              case _ => InvalidMove(s"Cannot move on top of Standing Stone at $index").left
            }
        }
    }
    newStack.map(setStackAt(positions, index, _))
  }
}

case class Board(size: Int, boardPositions: BoardLayout) {

  def applyAction(action: TurnAction): Checked[Board] = action match {
    case PlayStone(at, stone) =>
      stackAt(at).flatMap {
        case s if s.nonEmpty => InvalidMove(s"A stack already exists at ${at.name}").left
        case _ =>
          val stack = Stack.of(stone)
          val newPositions = setStackAt(boardPositions, at, stack)
          Board(size, newPositions).right
      }
    case m: Move => doMoveAction(m)
  }

  def applyActions(actions: Seq[TurnAction]): Checked[Board] =
    actions.headOption
      .toRightDisjunction(InvalidMove("Tried to apply an empty seq of actions"))
      .flatMap(a => applyActions(a, actions.tail: _*))

  @tailrec
  final def applyActions(a: TurnAction, as: TurnAction*): Checked[Board] =
    // Explicit match instead of map/flatmap to appease @tailrec
    applyAction(a) match {
      case e @ -\/(_) => e
      case s @ \/-(newState) =>
        as.toList match {
          case Nil => s
          case nextMove :: moreMoves => newState.applyActions(nextMove, moreMoves: _*)
        }
    }

  private[taklib] def doMoveAction(m: Move): Checked[Board] = {
    @tailrec
    def spreadStack(movingStack: Vector[Stone],
                    index: BoardIndex,
                    drops: List[Int],
                    positions: BoardLayout): Checked[BoardLayout] =
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

    def moveStack(stack: Stack, count: Int) = {
      val (remainingStack, movingStack) =
        stack.pieces.splitAt(stack.size - count)
      assert(
        movingStack.length == count,
        s"""Error applying $m to Board.fromTps("${this.toTps}")\nThis is an internal bug - please report it on github"""
      )

      val positionsWithoutMovedStones =
        setStackAt(boardPositions, m.from, Stack(remainingStack))
      val finalPositions =
        spreadStack(
          movingStack,
          m.from.neighbor(m.direction),
          m.drops.getOrElse(Vector(count)).toList,
          positionsWithoutMovedStones
        )
      finalPositions
    }

    // todo clean duplication in moveStack/spreadStack, maybe prepend an extra drop on
    for {
      stack <- stackAt(m.from)
      count = m.count.getOrElse(stack.size)
      _ <- (count <= size).guard(
        InvalidMove(s"Move wants to carry $count, which is larger than the board size ($size)")
      )
      _ <- stack.nonEmpty.guard(InvalidMove(s"Cannot move empty stack at $m.from"))
      finalPositions <- moveStack(stack, count)
    } yield Board(size, finalPositions)
  }

  def stackAt(index: BoardIndex): Checked[Stack] =
    \/.fromTryCatchNonFatal(boardPositions(index.file - 1)(index.rank - 1))
      .leftMap(_ => InvalidMove(s"$index is not on the board"))

  def hasIndex(index: BoardIndex): Boolean =
    index.file <= size && index.file >= 0 && index.rank <= size && index.rank >= 0

  /** Serialize board state to Tak Positional System */
  def toTps: String = {
    val rows = boardPositions.map { row =>
      row.map(_.toTps)
    }
    rows.map(_.mkString(",")).mkString("/")
  }
}

object BoardIndex {
  private[taklib] val rankNames = Vector('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
}
// Rank is normally 'a'..'e'/'f' depending on board size, Int here for convenience.
case class BoardIndex(file: Int, rank: Int) {
  def oppositeIndexes(boardSize: Int): IndexedSeq[BoardIndex] = {
    val left =
      if (file == 1) for (n <- 1 to boardSize) yield BoardIndex(boardSize, n) else Vector.empty
    val bottom =
      if (rank == 1) for (n <- 1 to boardSize) yield BoardIndex(n, boardSize) else Vector.empty
    val right =
      if (file == boardSize) for (n <- 1 to boardSize) yield BoardIndex(1, n) else Vector.empty
    val top =
      if (rank == boardSize) for (n <- 1 to boardSize) yield BoardIndex(n, 1) else Vector.empty
    left ++ bottom ++ right ++ top
  }

  import BoardIndex._
  // This will throw for larger than 8x8 but that's not even defined in the rules anyway
  def name: String = s"${rankNames(file - 1)}$rank"
  def neighbor(d: MoveDirection): BoardIndex =
    d match {
      case Left => copy(file = file - 1)
      case Right => copy(file = file + 1)
      case Up => copy(rank = rank + 1)
      case Down => copy(rank = rank - 1)
    }
  def allNeighbors(boardSize: Int): List[BoardIndex] =
    List(neighbor(Left), neighbor(Right), neighbor(Up), neighbor(Down)).filter { idx =>
      idx.file >= 1 && idx.file <= boardSize && idx.rank >= 1 && idx.rank <= boardSize
    }
}

object Stack {
  val empty = Stack(Vector.empty[Stone])
  def of(s: Stone) = Stack(Vector(s))
}
case class Stack(pieces: Vector[Stone]) {
  def controller: Option[Player] = top.map(_.owner)
  def top: Option[Stone] = pieces.lastOption
  def size: Int = pieces.size
  def isEmpty: Boolean = pieces.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def toTps: String =
    if (pieces.isEmpty) "x"
    else
      pieces.map {
        case FlatStone(owner) => owner.fold("2", "1")
        case StandingStone(owner) => owner.fold("2S", "1S")
        case Capstone(owner) => owner.fold("2C", "1C")
      } mkString ""
}

object Player {
  implicit val playerInstance: Equal[Player] = new Equal[Player] {
    override def equal(a1: Player, a2: Player): Boolean = a1 == a2
  }
}
sealed trait Player {
  def fold[A](ifBlack: => A, ifWhite: => A): A = this match {
    case Black => ifBlack
    case White => ifWhite
  }
}
case object Black extends Player
case object White extends Player

sealed trait Stone {
  val owner: Player
  val isRoadStone: Boolean
}
case class Capstone(owner: Player) extends Stone { val isRoadStone = true }
case class StandingStone(owner: Player) extends Stone { val isRoadStone = false }
case class FlatStone(owner: Player) extends Stone { val isRoadStone = true }
