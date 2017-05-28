package com.github.daenyth.opentak.protocol

import com.github.daenyth.opentak.accounts.Username
import com.github.daenyth.opentak.protocol.Playtak.GameNumber
import com.github.daenyth.taklib.White
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class PlaytakCodecTest extends FlatSpec with Matchers with EitherValues {
  import Playtak.Incoming._
  import PlaytakCodec.Incoming._

  def parse[A](parser: PlaytakCodec.Incoming.Parser[A], s: String): Either[String, A] =
    PlaytakCodec.Incoming.parseEither(parser, s)

  "client" should "parse" in {
    parse(client, "Client Foop") shouldBe Right(Client("Foop"))
  }

  "register" should "parse" in {
    parse(register, "Register Daenyth daenyth@gmail.com") shouldBe Right(
      Register(
        Username("Daenyth"),
        "daenyth@gmail.com"
      )
    )
  }

  "seek" should "parse" in {
    parse(seek, "Seek 5 600 0") shouldBe Right(Seek(5, 600, 0, None))
    parse(seek, "Seek 5 600 0 W") shouldBe Right(Seek(5, 600, 0, Some(White)))
  }

  "accept" should "parse" in {
    parse(accept, "Accept 123456") shouldBe Right(Accept(GameNumber(123456)))
  }
}
