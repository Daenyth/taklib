package com.github.daenyth.taklib

import scala.util.parsing.combinator.Parsers
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

object Implicits {
  implicit class RichBoolean(b: Boolean) {
    def guard[E](whenFalse: => E): E \/ Unit =
      if (b) ().right else whenFalse.left

    def orElse[A](ifTrue: => A): Option[A] =
      if (!b) Some(ifTrue) else None
  }

  // Credit to http://stackoverflow.com/a/27513509/350351
  trait RichParsing { this: Parsers =>
    implicit class RichParser[+T](p: Parser[T]) {
      def ^^?[U](f: T => \/[String, U]): Parser[U] = new Parser[U] {
        def apply(in: Input) = p(in) match {
          case Success(x, in1) => f(x) match {
            case -\/(error) => Failure(error, in1)
            case \/-(x1) => Success(x1, in1)
          }
          case failure: Failure => failure
          case error: Error => error
        }
      }
    }
  }
}
