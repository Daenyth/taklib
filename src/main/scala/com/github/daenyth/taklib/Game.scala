package com.github.daenyth.taklib

import com.github.daenyth.taklib.BoardState.Checked
import com.github.daenyth.taklib.BooleanOps._

import scala.annotation.tailrec
import scala.collection.immutable.{::, Nil}
import scalaz.Ordering.{EQ, GT, LT}
import scalaz.std.anyVal.intInstance
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.either._
import scalaz.syntax.foldable._
import scalaz.syntax.order._
import scalaz.syntax.semigroup._
import scalaz.{NonEmptyList, Semigroup}

object GameEndResult {
  implicit val gerInstance: Semigroup[GameEndResult] = new Semigroup[GameEndResult] {
    override def append(f1: GameEndResult, f2: => GameEndResult) = (f1, f2) match {
      case (DoubleRoad, _) => DoubleRoad
      case (_, DoubleRoad) => DoubleRoad
      case (Draw, _) => Draw
      case (_, Draw) => Draw
      case (r @ RoadWin(p1), RoadWin(p2)) => if (p1 == p2) r else DoubleRoad
      case (f @ FlatWin(p1), FlatWin(p2)) => if (p1 == p2) f else Draw
      case (r: RoadWin, _: FlatWin) => r
      case (_: FlatWin, r: RoadWin) => r
    }
  }
}
sealed trait GameEndResult
sealed trait RoadResult extends GameEndResult
sealed trait FlatResult extends GameEndResult
case class RoadWin(player: Player) extends RoadResult
case class FlatWin(player: Player) extends FlatResult
case object DoubleRoad extends RoadResult
case object Draw extends FlatResult

object Game {
  def actionIndexIsValid(board: BoardState, action: TurnAction): Boolean =
    action match {
      case play: PlayStone => board.hasIndex(play.at)
      case m: Move => board.hasIndex(m.from) && board.hasIndex(m.finalPosition)
    }
  def apply(size: Int): Game = {
    val b = BoardState.empty(size)
    Game(size, NonEmptyList((StartGameWithBoard(b), b)))
  }
  def apply(board: BoardState): Game =
    Game(board.size, NonEmptyList((StartGameWithBoard(board), board)))

  /** board size -> (stones, capstones) */
  val reserveSize: Map[Int, (Int, Int)] = Map(
    3 -> ((10, 0)),
    4 -> ((15, 0)),
    5 -> ((21, 1)),
    6 -> ((30, 1)),
    8 -> ((50, 2))
  )
}

// TODO Eventually change NEL to a tree zipper to allow for branching game history (unlimited rollback-rollforward)
case class Game private (size: Int, history: NonEmptyList[(GameAction, BoardState)]) {
  import Game._
  def currentBoard: BoardState = history.head._2
  def nextPlayer: Player = history.head._1 match {
    case StartGameWithBoard(_) => White
    case a: TurnAction =>
      a.player match {
        case White => Black
        case Black => White
      }
  }

  def turnNumber: Int = ???

  def takeTurn(action: TurnAction): Checked[Game] =
    for {
      _ <- moveIsValid(action)
      nextState <- currentBoard.applyAction(action)
      newHistory = (action, nextState) <:: history
    } yield this.copy(history = newHistory)

  def moveIsValid(action: TurnAction): Checked[Unit] =
    (
      action.player == nextPlayer
        && actionIndexIsValid(currentBoard, action)
    ).guard(InvalidMove)

  def undo: Checked[Game] =
    history.tail.toNel.map { prev =>
      this.copy(history = prev).right
    } getOrElse InvalidMove.left

  /** Serialize game history to Portable Tak Notation */
  def toPTN: String = ???

  def winner: Option[GameEndResult] =
    (roads: Vector[GameEndResult]).suml1Opt |+| flatWin

  private def roads: Vector[RoadWin] = ???

  private[taklib] def flatWin: Option[FlatResult] = {
    val allStacks = currentBoard.boardPositions.flatten.toList
    @tailrec
    def go(stacks: List[Stack],
           whiteFlats: Int,
           blackFlats: Int,
           whiteCount: Int,
           blackCount: Int,
           emptySpaceAvailable: Boolean): Option[FlatResult] =
      stacks match {
        case Nil =>
          val reserve = { val r = reserveSize(size); r._1 + r._2 }
          // TODO carry reserve stones left on the game object
          if (!emptySpaceAvailable
              || whiteCount == reserve
              || blackCount == reserve) {
            Some(whiteFlats cmp blackFlats match {
              case LT => FlatWin(Black)
              case EQ => Draw
              case GT => FlatWin(White)
            })
          } else None
        case stack :: rest =>
          val (whiteStones, blackStones) = stack.pieces.foldRight((0, 0)) { (stone, acc) =>
            stone.owner.fold((acc._1, acc._2 + 1), (acc._1 + 1, acc._2))
          }
          val (newWhiteFlats, newBlackFlats) = stack.top.fold((whiteFlats, blackFlats)) {
            case Capstone(_) => (whiteFlats, blackFlats)
            case StandingStone(_) => (whiteFlats, blackFlats)
            case FlatStone(owner) =>
              owner.fold((whiteFlats, blackFlats + 1), (whiteFlats + 1, blackFlats))
          }
          go(
            rest,
            newWhiteFlats,
            newBlackFlats,
            whiteCount + whiteStones,
            blackCount + blackStones,
            emptySpaceAvailable || stack.isEmpty
          )
      }
    go(allStacks, 0, 0, 0, 0, false)

  }

}
