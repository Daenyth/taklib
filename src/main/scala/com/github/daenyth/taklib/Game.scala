package com.github.daenyth.taklib

import com.github.daenyth.taklib.BoardState.Checked

import scalaz.NonEmptyList
import scalaz.syntax.either._

object BoardIndex {
  private[taklib] val rankNames = Vector('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
}
// Rank is normally 'a'..'e'/'f' depending on board size, Int here for convenience.
case class BoardIndex(rank: Int, file: Int) {
  import BoardIndex._
  // This will throw for larger than 8x8 but that's not even defined in the rules anyway
  def name: String = s"${rankNames(rank - 1)}$file"
  def neighbor(d: MoveDirection): BoardIndex =
    d match {
      case Left => copy(rank = rank - 1)
      case Right => copy(rank = rank + 1)
      case Up => copy(file = file + 1)
      case Down => copy(file = file - 1)
    }
}

sealed trait MoveDirection { def name: String }
case object Left extends MoveDirection { val name = "<" }
case object Right extends MoveDirection { val name = ">" }
case object Up extends MoveDirection { val name = "+" }
case object Down extends MoveDirection { val name = "-" }

case object InvalidMove

sealed trait GameAction
case class StartGameWithBoard(board: BoardState) extends GameAction
sealed trait TurnAction extends GameAction {
  def player: Player  // TODO maybe get rid of this - makes ptn parsing hard

  def ptn: String = this match {
    case PlayFlat(_, at) => at.name
    case PlayStanding(_, at) => s"S${at.name}"
    case PlayCapstone(_, at) => s"C${at.name}"
    case Move(_, from, direction, count, drops) =>
      // Omit count+drops if moving whole stack or one piece
      val num =
        drops match {
          case Some(ds) if ds.length > 1 => count.map(_.toString).getOrElse("")
          case _ => ""
        }
      val dropSequence =
        drops match {
          case Some(ds) if ds.length > 1 => ds.mkString("")
          case _ => ""
        }
      s"$num${from.name}${direction.name}$dropSequence"
  }
}
object PlayStone {
  def unapply(p: PlayStone): Option[(BoardIndex, Stone)] =
    Some((p.at, p.stone))
}
sealed trait PlayStone extends TurnAction {
  def at: BoardIndex
  def stone: Stone
}
case class PlayFlat(player: Player, at: BoardIndex) extends PlayStone {
  val stone = FlatStone(player)
}
case class PlayStanding(player: Player, at: BoardIndex) extends PlayStone {
  val stone = StandingStone(player)
}
case class PlayCapstone(player: Player, at: BoardIndex) extends PlayStone {
  val stone = Capstone(player)
}
object Move {
  def fromPtn(ptn: String): Option[Move] = ???
}
case class Move(player: Player,
                from: BoardIndex,
                direction: MoveDirection,
                count: Option[Int],
                drops: Option[Vector[Int]])
    extends TurnAction {
  def finalPosition: BoardIndex = {
    val moveDistance = drops.map(_.length).getOrElse(1)
    direction match {
      case Left => from.copy(rank = from.rank - moveDistance)
      case Right => from.copy(rank = from.rank + moveDistance)
      case Up => from.copy(file = from.file + moveDistance)
      case Down => from.copy(file = from.file - moveDistance)
    }
  }
}

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
    if (action.player == nextPlayer && actionIndexIsValid(currentBoard, action))
      ().right
    else InvalidMove.left

  def undo: Checked[Game] =
    history.tail.toNel.map { prev =>
      this.copy(history = prev).right
    } getOrElse InvalidMove.left

  /** Serialize game history to Portable Tak Notation */
  def toPTN: String = ???
}
