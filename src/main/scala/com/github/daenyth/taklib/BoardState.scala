package com.github.daenyth.taklib

import scalaz.NonEmptyList

object BoardState {
  /** Build a board from Tak Positional System; None if tps is invalid */
  def fromTPS(tps: String): Option[BoardState] = ???

  def empty(size: Int): BoardState = BoardState(size, Vector.fill(size, size)(EmptySpace))
}
case class BoardState(size: Int, boardPositions: Vector[Vector[BoardPosition]]) {
  def applyAction(action: TurnAction): BoardState = ???

  def hasIndex(index: BoardIndex): Boolean =
    index.rank < size && index.rank >= 0 && index.file < size && index.file >= 0
}

sealed trait BoardPosition
case object EmptySpace extends BoardPosition
case class Stack(pieces: NonEmptyList[Stone]) extends BoardPosition {
  def controller: Player = pieces.last.owner
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
