package com.github.daenyth.taklib

import BoardState.Checked
import BooleanOps._

import scalaz.NonEmptyList
import scalaz.syntax.either._



sealed trait GameEndResult
case class RoadWin(player: Player) extends GameEndResult
case class FlatWin(player: Player) extends GameEndResult
case object DoubleRoad extends GameEndResult
case object Draw

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
}
