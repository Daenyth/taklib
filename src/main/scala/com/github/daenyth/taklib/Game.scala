package com.github.daenyth.taklib

import com.github.daenyth.taklib.Board.Checked
import com.github.daenyth.taklib.BooleanOps._

import scala.annotation.tailrec
import scala.collection.immutable.{::, Nil}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalaz.Ordering.{EQ, GT, LT}
import scalaz.std.anyVal.intInstance
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.either._
import scalaz.syntax.foldable._
import scalaz.syntax.order._
import scalaz.syntax.semigroup._
import scalaz.syntax.std.option._
import scalaz.{Equal, NonEmptyList, Semigroup}

object GameEndResult {
  implicit val gerInstance: Semigroup[GameEndResult] with Equal[GameEndResult] =
    new Semigroup[GameEndResult] with Equal[GameEndResult] {
      override def append(f1: GameEndResult, f2: => GameEndResult) = (f1, f2) match {
        case (DoubleRoad, _) => DoubleRoad
        case (_, DoubleRoad) => DoubleRoad
        case (Draw, r: RoadWin) => r
        case (r: RoadWin, Draw) => r
        case (r @ RoadWin(p1), RoadWin(p2)) => if (p1 == p2) r else DoubleRoad
        case (f @ FlatWin(p1), FlatWin(p2)) => if (p1 == p2) f else Draw
        case (Draw, _) => Draw
        case (_, Draw) => Draw
        case (r: RoadWin, _: FlatWin) => r
        case (_: FlatWin, r: RoadWin) => r
      }

      override def equal(a1: GameEndResult, a2: GameEndResult): Boolean = (a1, a2) match {
        case (DoubleRoad, DoubleRoad) => true
        case (Draw, Draw) => true
        case (RoadWin(p1), RoadWin(p2)) => p1 == p2
        case (FlatWin(p1), FlatWin(p2)) => p1 == p2
        case _ => false
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
  def actionIndexIsValid(board: Board, action: TurnAction): Boolean =
    action match {
      case play: PlayStone => board.hasIndex(play.at)
      case m: Move => board.hasIndex(m.from) && board.hasIndex(m.finalPosition)
    }
  def actingPlayerControlsStack(board: Board, action: TurnAction): Boolean =
    action match {
      case play: PlayStone => true
      case m: Move =>
        val equal = for {
          stack <- board.stackAt(m.from)
          controller <- stack.controller.toRightDisjunction(InvalidMove)
        } yield controller === action.player
        equal.getOrElse(false)
    }
  def ofSize(size: Int): Game = {
    val b = Board.ofSize(size)
    Game(size, 1, NonEmptyList((StartGameWithBoard(b), b)))
  }

  // Start at turn 3 to make the "play opponent's stone" rule easier
  def fromBoard(board: Board): Game =
    Game(board.size, 3, NonEmptyList((StartGameWithBoard(board), board)))

  def fromPtn(ptn: String): Option[Game] = ???

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
case class Game private (size: Int, turnNumber: Int, history: NonEmptyList[(GameAction, Board)]) {
  import Game._
  def currentBoard: Board = history.head._2
  def nextPlayer: Player = turnNumber match {
      case 1 => Black
      case 2 => White
      case n if n % 2 == 1 => White
      case _ => Black
  }

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
        && actingPlayerControlsStack(currentBoard, action)
    ).guard(InvalidMove)

  def undo: Checked[Game] =
    history.tail.toNel.map { prev =>
      this.copy(turnNumber = this.turnNumber - 1, history = prev).right
    } getOrElse InvalidMove.left

  /** Serialize game history to Portable Tak Notation */
  def toPTN: String = ???

  def winner: Option[GameEndResult] =
    (roads: Vector[GameEndResult]).suml1Opt |+| flatWin

  private def roads: Vector[RoadWin] = {
    def mkGraph(xs: Set[BoardIndex]): Graph[BoardIndex, UnDiEdge] = {
      val edges = for {
        idx <- xs
        n <- idx.allNeighbors(size)
        if xs.contains(n)
      } yield UnDiEdge(idx, n)
      Graph.from(xs, edges)
    }
    val roadStones = for {
      rank <- 1 to size
      file <- 1 to size
      index = BoardIndex(rank, file)
      stack <- currentBoard.stackAt(index).toList
      top <- stack.top.toList
      if top.isRoadStone
    } yield (index, top.owner)
    val (whiteRoadStones, blackRoadStones) = roadStones.partition { _._2 == White }
    val whiteIndexes = whiteRoadStones.map(_._1)
    val blackIndexes = blackRoadStones.map(_._1)
    val whiteGraph = mkGraph(whiteIndexes.toSet)
    val blackGraph = mkGraph(blackIndexes.toSet)
    val edgeIndexes = for {
      rank <- 1 to size
      file <- 1 to size
      if rank == 1 || file == 1 || rank == size || file == size
    } yield BoardIndex(rank, file)
    def getEdgePath(g: Graph[BoardIndex, UnDiEdge]): IndexedSeq[g.Path] =
      for {
        edge <- edgeIndexes
        opposite <- edge.oppositeIndexes(size)
        startNode <- g.find(edge).toList
        endNode <- g.find(opposite).toList
        path <- startNode.pathTo(endNode)
      } yield path
    val whitePaths = getEdgePath(whiteGraph)
    val blackPaths = getEdgePath(blackGraph)
    Vector((White, whitePaths.nonEmpty), (Black, blackPaths.nonEmpty)).flatMap {
      case (player, hasRoad) =>
        if (hasRoad) Vector(RoadWin(player)) else Vector.empty
    }
  }

  private def flatWin: Option[FlatResult] = {
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
