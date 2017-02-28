package com.github.daenyth.taklib

import scala.language.implicitConversions

import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait MoveResultValues {
  implicit def toMoveResultValue[A](m: MoveResult[A])(implicit pos: source.Position) =
    new MoveResultValue[A](m, pos)

  class MoveResultValue[A](m: MoveResult[A], pos: source.Position) {
    def value: A = m match {
      case OkMove(ok) => ok
      case GameOver(o) =>
        throw new TestFailedException((_: StackDepthException) => Some(s"$o is GameOver, expected OkMove."), None, pos)
      case InvalidMove(i) =>
        throw new TestFailedException((_: StackDepthException) => Some(s"$i is InvalidMove, expected OkMove."), None, pos)
    }
  }
}
