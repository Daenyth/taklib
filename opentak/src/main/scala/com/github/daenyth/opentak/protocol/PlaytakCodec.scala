package com.github.daenyth.opentak.protocol

object PlaytakCodec {

  object Incoming {
    def decode(incoming: String): Either[String, Playtak.Incoming] = ???
  }

  object Outgoing {
    def encode(outgoing: Playtak.Outgoing): String = ???
  }
}
