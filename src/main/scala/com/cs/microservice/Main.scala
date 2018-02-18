package com.cs.microservice

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.cs.microservice.http.{RestService, TechTestServiceRoutes}
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal


sealed trait CardServiceCake extends SysProvider with TechTestServiceRoutes with RestService with ServiceConfig {
  this: Sys =>
  override def serviceApi: Route = cardsRoute
}

object Main extends App {
  private lazy val system: ActorSystem = ActorSystem()

  try {
    val sys = new SysProvider()(system, ActorMaterializer()(system)) with CardServiceCake
    sys.start()
  } catch {
    case NonFatal(e) =>
      LoggerFactory.getLogger(getClass).error("Failed to initialise service", e)
      system.terminate()
  }
}
