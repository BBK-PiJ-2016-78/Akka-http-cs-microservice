package com.cs.microservice.http

import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.cs.microservice.{CreditCard, FormInput, ServiceConfig, Sys}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.math.pow

trait TechTestServiceRoutes {
  this: Sys with ServiceConfig =>

  import akka.http.scaladsl.server.Directives._
  import com.cs.microservice.ServiceJsonProtocol._
  import spray.json._

  private var cards: List[CreditCard] = Nil

  private val pingRoute = pathPrefix("ping") {
    get {
      complete(Future.successful(Map("hello" -> "world", "status" -> "up")).map(_.toJson))
    }
  }

  /**
    * Call findCreditCards to collect all results and return a json
    * as a response containing the results. If endpoints are not set it will
    * return a message. Reset the value of the cards variable so that every time
    * the endpoint is called we get the right results.
    */
  private val postCardsRoute = pathPrefix("creditcards") {
    post {
      entity(as[FormInput]) {in =>
        cards = Nil
        findCreditCards(in)
        complete(if(cards.nonEmpty) cards else "No eligible credit cards found!")
      }
    }
  }

  /**
    * Do a POST request to external service using the AKKA HTTP Client API.
    * The input needs to be serialized with a Marshaller to be converted into
    * RequestEntity that can be send and converted to Json on the way.
    * @param url the endpoint to be called which is defined in environment variable
    * @param in FormInput which is used to represent user entered information
    * @return Future[HttpResponse] which holds the response of the request
    */
  private def postDataToEndpoint(url: String, in : FormInput): Future[HttpResponse] =
    Marshal(in).to[RequestEntity] flatMap { entity =>
      Http(system).singleRequest(HttpRequest(HttpMethods.POST, url, entity = entity))
    }

  /**
    * Concatenate the results which will be List[CreditCard] into a list.
    * The result of the Future is obtained by calling Await.result().
    * For each incoming result we need to adjust the eligibility score.
    * @param url the endpoint to be called which is defined in environment variable
    * @param in FormInput which is used to represent user entered information
    * @param multiplier scala that normalizes the eligibility score of different cards
    * @return Future with the operation to be done
    */
  private def collectResults(url: String, in: FormInput, multiplier: Int) = {
    val result = Await.result(postDataToEndpoint(url, in), 5.seconds)
    Unmarshal (result).to[List[CreditCard]].flatMap { data =>
      data.foreach(e => e.eligibility = e.eligibility * multiplier)
      Future.successful(cards = cards ::: data)
    }
  }

  /**
    * Collect all results from both endpoints. The first one requires that
    * fullName is set, whereas the second requires no fullName.
    * If the resulting List[CreditCard] is not empty, then calculate the
    * cardScore by the given formula. Sort the cards by the cardScore.
    * @param in FormInput which is used to represent user entered information
    */
  private def findCreditCards(in: FormInput) = {
    in.changeFullName(false)
    collectResults(csCardsEndPoint, in, 1)
    in.changeFullName(true)
    collectResults(scoredCardsEndPoint, in, 10)
    if(cards.nonEmpty) {
      cards.foreach(e => if(e.apr > 0) e.cardScore = Some(e.eligibility * pow(1 / e.apr, 2)))
      cards = cards.sortWith(_.cardScore.get > _.cardScore.get)
    }
  }

  val cardsRoute: Route = pingRoute ~ postCardsRoute
}
