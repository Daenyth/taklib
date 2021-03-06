package com.github.daenyth.taklib

import scala.util.parsing.combinator.RegexParsers
import cats.syntax.either._

object Implicits {
  implicit class RichBoolean(b: Boolean) {
    def guard[E](whenFalse: => E): Either[E, Unit] =
      if (b) ().asRight else whenFalse.asLeft

    def orElse[A](ifTrue: => A): Option[A] =
      if (!b) Some(ifTrue) else None
  }

  trait RichParsing { this: RegexParsers =>
    // Credit to http://stackoverflow.com/a/27513509/350351
    implicit class RichParser[+T](p: Parser[T]) {
      def ^^?[U](f: T => (Either[String, U])): Parser[U] = new Parser[U] {
        def apply(in: Input) = p(in) match {
          case Success(x, in1) =>
            f(x) match {
              case scala.Left(error) => Failure(error, in1)
              case scala.Right(x1) => Success(x1, in1)
            }
          case failure: Failure => failure
          case error: Error => error
        }
      }

      def >>=?[U](f: T => (Either[String, Parser[U]])): Parser[U] = p.flatMap { x =>
        f(x) match {
          case scala.Left(err) =>
            new Parser[U] { def apply(in: Input): ParseResult[U] = Failure(err, in) }
          case scala.Right(parser) => parser
        }
      }
    }

    // Credit to http://jim-mcbeath.blogspot.com/2011/07/debugging-scala-parser-combinators.html
    class DebugParser[+T](name: String, parser: Parser[T]) extends Parser[T] {
      def apply(in: Input): ParseResult[T] = {
        val first = in.first
        val pos = in.pos
        val offset = in.offset
        val t = parser.apply(in)
        println(s"$name.apply for token $first at pos $pos offset $offset returns '$t' leaving\n${t.next.pos.longString}")
        t
      }
    }

    /** Use like `"foo" ???> (fooParser: Parser[Foo])` */
    implicit class DebugParserOps(name: String) {
      def ???>[T](p: Parser[T]) = new DebugParser(name, p)
    }

    def parseEither[T](parser: this.Parser[T], input: String): Either[String, T] =
      this.parse(parser, input) match {
        case Success(result, _) => result.asRight
        case err: NoSuccess => err.msg.asLeft
      }
  }
}
