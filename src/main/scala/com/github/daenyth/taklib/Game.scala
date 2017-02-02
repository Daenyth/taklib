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

sealed trait TurnAction
sealed trait PlayStone extends TurnAction {
  def at: BoardIndex
}
case class PlayFlat(at: BoardIndex) extends PlayStone
case class PlayStanding(at: BoardIndex) extends PlayStone
case class PlayCapstone(at: BoardIndex) extends PlayStone
case class Move(from: BoardIndex,
                direction: MoveDirection,
                count: Int,
                drops: Vector[Int])
    extends TurnAction

case class Game(size: Int, history: NonEmptyList[(Move, BoardState)]) {
  def currentState: BoardState = history.head._2
  def nextPlayer: Player = if (history.size % 2 == 0) White else Black
  def takeTurn(action: TurnAction): InvalidMove.type \/ Game = for {
    _ <- actionIndexIsValid(currentState, action)
  } yield this.copy(history = ((action, applyAction(currentState, action)) <:: history)).right

  def undo: (InvalidMove.type \/ Game) =
    history.tail.toNel.map { prev =>
      this.copy(history = prev).right[InvalidMove.type]
    } getOrElse InvalidMove.left
}
