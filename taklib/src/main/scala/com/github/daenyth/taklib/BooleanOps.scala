package com.github.daenyth.taklib

import scalaz.\/
import scalaz.syntax.either._

object BooleanOps {
  implicit class RichBoolean(b: Boolean) {
    def guard[E](whenFalse: => E): E \/ Unit =
      if (b) ().right else whenFalse.left

    def orElse[A](ifTrue: => A): Option[A] =
      if (!b) Some(ifTrue) else None
  }
}
