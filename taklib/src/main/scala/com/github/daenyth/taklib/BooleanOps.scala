package com.github.daenyth.taklib

import scalaz.\/
import scalaz.syntax.either._

object BooleanOps {
  implicit class BooleanEither(b: Boolean) {
    def guard[E](whenFalse: => E): E \/ Unit =
      if (b) ().right else whenFalse.left
  }
}
