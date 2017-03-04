package com.github.daenyth.taklib

import scala.util.parsing.combinator.RegexParsers
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

object Implicits {
  implicit class RichBoolean(b: Boolean) {
    def guard[E](whenFalse: => E): E \/ Unit =
      if (b) ().right else whenFalse.left

    def orElse[A](ifTrue: => A): Option[A] =
      if (!b) Some(ifTrue) else None
  }

  trait RichParsing { this: RegexParsers =>
    // Credit to http://stackoverflow.com/a/27513509/350351
    implicit class RichParser[+T](p: Parser[T]) {
      def ^^?[U](f: T => (String \/ U)): Parser[U] = new Parser[U] {
        def apply(in: Input) = p(in) match {
          case Success(x, in1) => f(x) match {
            case -\/(error) => Failure(error, in1)
            case \/-(x1) => Success(x1, in1)
          }
          case failure: Failure => failure
          case error: Error => error
        }
      }

      def >>?[U](f: T => (String \/ Parser[U])): Parser[U] = p.flatMap { x =>
        f(x) match {
          case -\/(err) => new Parser[U] { def apply(in: Input): ParseResult[U] = Failure(err, in) }
          case \/-(parser) => parser
        }
      }
    }

    def parseEither[T](parser: this.Parser[T], input: String): String \/ T =
      this.parse(parser, input) match {
        case Success(result, _) => \/.right(result)
        case err: NoSuccess =>
          \/.left(err.msg)
      }
  }
}
