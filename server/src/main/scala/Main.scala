package com.alecdorrington.server

import cats.effect.{IO, Resource, ResourceApp}
import cats.syntax.all.*
import com.alecdorrington.server.config.Env
import com.alecdorrington.server.services.{Assembly, CoreService, PollService}
import com.alecdorrington.server.store.Polls
import sttp.tapir.server.netty.cats.NettyCatsServer

object Main extends ResourceApp.Forever:

  /**
    * The main entry point for this application. To run, use "sbt dev".
    *
    * @param args
    *   Unused.
    */
  def run(args: List[String]) =

    for
      polls  <- Resource.eval(Polls.open(Env.DATA_FILE))
      server <- NettyCatsServer.io()
      configured = server
        .host(Env.HOST)
        .port(Env.PORT)
        .addEndpoints(Assembly.endpoints(List(CoreService, PollService(polls))))
      _ <- Resource.make(configured.start())(_.stop())
      _ <- Resource.eval(IO.println(s"Kairos listening on ${ Env.HOST }:${ Env
          .PORT }"))
    yield ()
