package com.github.daenyth.taklib

import scalaz.NonEmptyList

case class BoardState(size: Int, boardPositions: Vector[Vector[BoardPosition]])

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
