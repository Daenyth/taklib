package com.github.daenyth.taklib

import com.github.daenyth.taklib.BooleanOps._
import com.github.daenyth.taklib.RuleSet.GameRule

import scala.annotation.tailrec
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
import scalaz.{Equal, NonEmptyList, Semigroup, \/}

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

object RuleSet {
  type GameRule = (Game, TurnAction) => Option[InvalidMove]
}

trait RuleSet {

  def check(game: Game, action: TurnAction): Option[InvalidMove] =
    rules.view.map(_(game, action)).collectFirst {
      case Some(reason) => reason
    }

  val rules: Vector[GameRule]

  /** board size -> (stones, capstones) */
  val stoneCounts: Map[Int, (Int, Int)]
}

object DefaultRules extends RuleSet {

  val actionIndexIsValid: GameRule = { (game, action) =>
    val board = game.currentBoard
    action match {
      case play: PlayStone =>
        board.hasIndex(play.at).orElse(InvalidMove(s"${play.at} is not on the board"))
      case m: Move =>
        val hasStart = board
          .hasIndex(m.from)
          .orElse(InvalidMove(s"${m.from} is not on the board"))
        val hasEnd = board
          .hasIndex(m.finalPosition)
          .orElse(InvalidMove(s"Move final position ${m.finalPosition} is not on the board"))
        hasStart orElse hasEnd
    }
  }

  val actingPlayerControlsStack: GameRule = { (game, action) =>
    val board = game.currentBoard
    action match {
      case play: PlayStone => None
      case m: Move =>
        board
          .stackAt(m.from) match {
          case i: InvalidMove => Some(i)
          case _: GameOver => None
          case OkMove(stack) =>
            stack.controller match {
              case None =>
                Option(InvalidMove(s"Cannot move empty Stack at ${m.from}"))
              case Some(controller) =>
                (controller === action.player)
                  .orElse(
                    InvalidMove(
                      s"${action.player} cannot move stack controlled by $controller at ${m.from}"
                    )
                  )
            }

        }
    }
  }

  val playerOwnsStone: GameRule = { (game, action) =>
    (action.player == game.nextPlayer)
      .orElse(InvalidMove(s"${action.player} is not the correct color for this turn"))
  }

  override val rules: Vector[GameRule] = Vector(
    actionIndexIsValid,
    actingPlayerControlsStack,
    playerOwnsStone
  )

  override val stoneCounts: Map[Int, (Int, Int)] = Map(
    3 -> ((10, 0)),
    4 -> ((15, 0)),
    5 -> ((21, 1)),
    6 -> ((30, 1)),
    8 -> ((50, 2))
  )
}

object Game {

  def ofSize(size: Int): String \/ Game = ofSize(size, DefaultRules)

  def ofSize(size: Int, rules: RuleSet): String \/ Game =
    rules.stoneCounts.keySet
      .contains(size)
      .guard(s"Bad game size: $size")
      .map { _ =>
        val b = Board.ofSize(size)
        new Game(size, 1, rules, NonEmptyList((StartGameWithBoard(b), b)))
      }

  // Start at turn 3 to make the "play opponent's stone" rule easier
  def fromBoard(board: Board, turnNumber: Int = 3): Game =
    new Game(
      board.size,
      turnNumber,
      DefaultRules,
      NonEmptyList((StartGameWithBoard(board), board))
    )

  def fromPtn(ptn: String): String \/ Game = ???

  def fromTps(tps: String): String \/ Game =
    TpsParser.parse(TpsParser.board, tps) match {
      case TpsParser.Success((board, turn, move), _) =>
        // We use one turn for each player's action, Tps uses turn as a move for both players with a move counter between them
        val turnNumber = (2 * turn) + move - 1
        Game.fromBoard(board, turnNumber).right
      case err: TpsParser.NoSuccess => err.msg.left
    }

}

// TODO Eventually change NEL to a tree zipper to allow for branching game history (unlimited rollback-rollforward)
class Game private (val size: Int,
                    val turnNumber: Int,
                    val rules: RuleSet,
                    val history: NonEmptyList[(GameAction, Board)]) {

  private val reserveCount = rules.stoneCounts(size)
  def currentBoard: Board = history.head._2
  def nextPlayer: Player = turnNumber match {
    case 1 => Black
    case 2 => White
    case n if n % 2 == 1 => White
    case _ => Black
  }

  def takeTurn(action: TurnAction): MoveResult[Game] =
    rules.check(this, action).getOrElse {
      currentBoard.applyAction(action).map { nextState =>
        val newHistory = (action, nextState) <:: history
        new Game(size, turnNumber + 1, rules, newHistory)
      }
    }

  def undo: MoveResult[Game] =
    history.tail.toNel.map { prev =>
      OkMove(new Game(size, turnNumber - 1, rules, prev))
    } getOrElse InvalidMove("Cannot undo when at the beginning of the game")

  /** Serialize game history to Portable Tak Notation */
  def toPtn: String = ???

  /** Serialize the current board state to TPS */
  def toTps: String = {
    val turn: Int = turnNumber / 2
    val move = nextPlayer.fold(1, 2)
    val board = currentBoard.toTps
    s"[ $board $turn $move ]"
  }

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
    val roadStones: Vector[(BoardIndex, Player)] = currentBoard.stacksWithIndex.flatMap { case (idx, stack) =>
      stack.top match {
        case Some(stone) if stone.isRoadStone => Vector(idx -> stone.owner)
        case _ => Vector.empty
      }
    }
    val (whiteRoadStones, blackRoadStones) = roadStones.partition { _._2 == White }
    val whiteIndexes = whiteRoadStones.map(_._1)
    val blackIndexes = blackRoadStones.map(_._1)
    val whiteGraph = mkGraph(whiteIndexes.toSet)
    val blackGraph = mkGraph(blackIndexes.toSet)
    // We only look at two edges because getting the opposites for each position will cover the other edges
    val edgeIndexes = for {
      rank <- 1 to size
      file <- 1 to size
      if rank == 1 || file == 1
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
          val reserve = reserveCount._1 + reserveCount._2
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
