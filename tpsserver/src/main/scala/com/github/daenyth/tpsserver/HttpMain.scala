package com.github.daenyth.tpsserver

import org.http4s.blaze.server.BlazeServerBuilder
import cats.effect.IOApp
import cats.effect.IO

object HttpMain extends IOApp.Simple {
  override def run: IO[Unit] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(TpsServer.tpsService.orNotFound)
      .resource
      .useForever
}
