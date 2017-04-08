package com.github.daenyth.tpsserver

import fs2.{Stream, Task}
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.util.StreamApp

object HttpMain extends StreamApp {
  override def main(args: List[String]): Stream[Task, Nothing] =
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(TpsServer.tpsService, "/")
      .serve
}
