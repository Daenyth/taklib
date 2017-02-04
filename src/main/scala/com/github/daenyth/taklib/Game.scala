package com.github.daenyth.taklib

import scalaz.{NonEmptyList, \/}
import scalaz.syntax.either._

// Rank is normally 'a'..'e'/'f' depending on board size, Int here for convenience.
case class BoardIndex(rank: Int, file: Int)

sealed trait MoveDirection
case object Left extends MoveDirection
case object Right extends MoveDirection
case object Up extends MoveDirection
case object Down extends MoveDirection

case object InvalidMove

sealed trait GameAction
case class StartGameWithBoard(board: BoardState) extends GameAction
sealed trait TurnAction extends GameAction {
  def player: Player
}
sealed trait PlayStone extends TurnAction {
  def at: BoardIndex
}
case class PlayFlat(player: Player, at: BoardIndex) extends PlayStone
case class PlayStanding(player: Player, at: BoardIndex) extends PlayStone
case class PlayCapstone(player: Player, at: BoardIndex) extends PlayStone
case class Move(player: Player,
                from: BoardIndex,
                direction: MoveDirection,
                count: Int,
                drops: Vector[Int])
    extends TurnAction {
  def finalPosition: BoardIndex = {
    val moveDistance = drops.length
    direction match {
      case Left => from.copy(rank = from.rank - moveDistance)
      case Right => from.copy(rank = from.rank + moveDistance)
      case Up => from.copy(file = from.file + moveDistance)
      case Down => from.copy(file = from.file - moveDistance)
    }
  }
}

object Game {
  def actionIndexIsValid(board: BoardState, action: TurnAction): Boolean = {
    action match {
      case play: PlayStone => board.hasIndex(play.at)
      case m: Move => board.hasIndex(m.from) && board.hasIndex(m.finalPosition)
    }
  }
  def apply(size: Int): Game = {
    val b = BoardState.empty(size)
    Game(size, NonEmptyList((StartGameWithBoard(b), b)))
  }
  def apply(board: BoardState) =
    Game(board.size, NonEmptyList((StartGameWithBoard(board), board)))
}

// TODO Eventually change NEL to a tree zipper to allow for branching game history (unlimited rollback-rollforward)
case class Game private (size: Int,
                         history: NonEmptyList[(GameAction, BoardState)]) {
  import Game._
  def currentBoard: BoardState = history.head._2
  def nextPlayer: Player = history.last._1 match {
    case StartGameWithBoard(_) => White
    case a: TurnAction => if (a.player == White) Black else White
  }
  def takeTurn(action: TurnAction): InvalidMove.type \/ Game =
    if (moveIsValid(action))
      this
        .copy(history = (action, currentBoard.applyAction(action)) <:: history)
        .right
    else InvalidMove.left

  def moveIsValid(action: TurnAction): Boolean = {
    // TODO more checks
    actionIndexIsValid(currentBoard, action)
  }

  def undo: (InvalidMove.type \/ Game) =
    history.tail.toNel.map { prev =>
      this.copy(history = prev).right
    } getOrElse InvalidMove.left
}
