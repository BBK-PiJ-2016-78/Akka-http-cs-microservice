package com.cs.microservice

import java.lang.management.ManagementFactory

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContextExecutor

trait Sys {
  implicit def system: ActorSystem
  implicit lazy val dispatcher: ExecutionContextExecutor = system.dispatcher
  implicit def materializer: Materializer
  def createLogger(tag: String): Logger =
    if (tag.startsWith("com.cs.microservice")) LoggerFactory.getLogger(tag)
    else LoggerFactory.getLogger(s"com.cs.microservice.$tag")
}

trait ServiceConfig {
  final val started: Long = ManagementFactory.getRuntimeMXBean.getStartTime
  val raw: Config = ConfigFactory.load()
  val httpPort: Int = raw.getInt("test-service.port")
  val csCardsEndPoint: String = raw.getString("cs-cards.endpoint")
  val scoredCardsEndPoint: String = raw.getString("scored-cards.endpoint")
}

class SysProvider(implicit val system: ActorSystem, val materializer: Materializer) extends Sys
