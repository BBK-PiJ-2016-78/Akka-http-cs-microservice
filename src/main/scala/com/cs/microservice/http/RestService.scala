package com.cs.microservice.http

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.cs.microservice.{ServiceConfig, Sys}

import scala.util.{Failure, Success}

trait RestService {
  this: Sys with ServiceConfig =>

  private lazy val logger = createLogger("ServiceInitialiser")

  def serviceApi: Route

  final def start(): Unit = {
    logger.info(s"Attempting to start test-service")

    def createRoutes: Route = {
      import akka.http.scaladsl.server.Directives._
      logger.info("Initialising routes for service")
      pathPrefix("v1") {
        serviceApi
      }
    }

    val routes = createRoutes

    Http().bindAndHandle(handler = routes, interface = "0.0.0.0", port = httpPort) onComplete {
      case Success(binding) =>
        logger.info(s"Bound service to local port $httpPort. Adding shutdown hook")
        sys.addShutdownHook(
          () => {
            logger.info(s"Shutdown initiated. Unbinding port $httpPort")
            binding.unbind() onComplete {
              case Success(_) =>
                logger.debug(s"Shutdown successfully completed. Terminating system.")
                system.terminate()
              case Failure(e) =>
                logger.warn("Failed to shutdown successfully and unbind port properly", e)
                system.terminate()
            }
          }
        )
      case Failure(ex) =>
        logger.error("Failed to start http service.", ex)
        system.terminate()
    }

  }

}
