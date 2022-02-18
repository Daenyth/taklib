package com.github.daenyth.taklib

import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait MoveResultValues {
  implicit def toMoveResultValue[A](m: MoveResult[A])(implicit pos: source.Position) =
    new MoveResultValue[A](m, pos)

  class MoveResultValue[A](m: MoveResult[A], pos: source.Position) {
    def value: A = m match {
      case OkMove(ok) => ok
      case o: GameOver =>
        throw new TestFailedException(
          (_: StackDepthException) => Some(s"Got $o, expected OkMove."),
          None,
          pos
        )
      case i: InvalidMove =>
        throw new TestFailedException(
          (_: StackDepthException) => Some(s"Got $i, expected OkMove."),
          None,
          pos
        )
    }
  }
}
