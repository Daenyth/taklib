package com.github.daenyth.tpsserver

import fs2.Task
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.{Server, ServerApp}

object HttpMain extends ServerApp {
  override def server(args: List[String]): Task[Server] =
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(TpsServer.tpsService, "/")
      .start
}
