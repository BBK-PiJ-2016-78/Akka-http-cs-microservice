package com.cs.microservice.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.MalformedRequestContentRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.Materializer
import com.cs.microservice.{CreditCard, ServiceConfig, Sys}
import org.scalactic.Equality
import org.scalatest.{Matchers, WordSpec}
import spray.json.DeserializationException
/**
  * Created by hradev01.
  */
class TechTestServiceRoutesTest extends WordSpec with Matchers with ScalatestRouteTest {

  case class PostClass(sys: ActorSystem, mat: Materializer) extends TechTestServiceRoutes with Sys with ServiceConfig {
    override implicit def system: ActorSystem = sys
    override implicit def materializer: Materializer = mat
  }

  private val routes = PostClass(system, materializer).cardsRoute

  implicit val cardsEq = new Equality[List[CreditCard]] {
    override def areEqual(a: List[CreditCard], b: Any) = b match {
      case c: List[CreditCard] => (c, a) match {
        case (x :: _, f :: _) => x.cardName == f.cardName && x.url == f.url && x.apr == f.apr && x.eligibility == f.eligibility
        case _ => false
      }
      case _ => false
    }
  }

  import com.cs.microservice.ServiceJsonProtocol._

  "The service" should {

    "return json with all cards when full information input json is provided" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1991/04/18\", \"creditScore\": 341, \"employmentStatus\": \"PART_TIME\", \"salary\": 18500}")
      Post("/creditcards", httpEntity) ~> routes ~> check {
        responseAs[String] shouldEqual "[{\"cardName\":\"ScoredCard Builder\",\"url\":\"http://www.example.com/apply\",\"apr\":19.4,\"eligibility\":8.0,\"features\":[\"\\\"Supports ApplePay\\\",\\\"Interest free purchases for 1 month\\\"\"],\"card-score\":0.03},{\"cardName\":\"SuperSaver Card\",\"url\":\"http://www.example.com/apply\",\"apr\":21.4,\"eligibility\":6.31,\"card-score\":0.02},{\"cardName\":\"SuperSpender Card\",\"url\":\"http://www.example.com/apply\",\"apr\":19.21,\"eligibility\":5.0,\"features\":[\"\\\"Interest free purchases for 6 months\\\"\"],\"card-score\":0.02}]"
      }
    }

    "return json with only CS cards when partial input json is provided" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\", \"creditScore\": 500}")
      Post("/creditcards", httpEntity) ~> routes ~> check {
        responseAs[String] shouldEqual "[{\"cardName\":\"SuperSaver Card\",\"url\":\"http://www.example.com/apply\",\"apr\":21.4,\"eligibility\":6.31,\"card-score\":0.02},{\"cardName\":\"SuperSpender Card\",\"url\":\"http://www.example.com/apply\",\"apr\":19.21,\"eligibility\":5.0,\"features\":[\"\\\"Interest free purchases for 6 months\\\"\"],\"card-score\":0.02}]"
      }
    }

    "return list of CreditCards for CS and compare to expected result" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\", \"creditScore\": 500}")
      Post("/creditcards", httpEntity) ~> routes ~> check {
        entityAs[List[CreditCard]] should === (List(new CreditCard("SuperSaver Card", "http://www.example.com/apply", 21.4f, 6.31f, None, Some(0.02d)), new CreditCard("SuperSpender Card", "http://www.example.com/apply", 19.21f, 5f, Some(List("Interest free purchases for 6 months")), Some(0.02d))))
      }
    }

    "return list of CreditCards for Scored and compare to expected result" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1991/04/18\", \"creditScore\": 341, \"employmentStatus\": \"PART_TIME\", \"salary\": 18500}")
      Post("/creditcards", httpEntity) ~> routes ~> check {
        entityAs[List[CreditCard]] should === (List(new CreditCard("ScoredCard Builder", "http://www.example.com/apply", 19.4f, 8f, Some(List("Supports ApplePay", "Interest free purchases for 1 month")), Some(0.03d)), new CreditCard("SuperSaver Card", "http://www.example.com/apply", 21.4f, 6.31f, None, Some(0.02d)), new CreditCard("SuperSpender Card", "http://www.example.com/apply", 19.21f, 5f, Some(List("Interest free purchases for 6 months")), Some(0.02d))))
      }
    }

    "return rejection message if malformed json is used as input" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\"}")
      Post("/creditcards", httpEntity) ~> routes ~> check {
        rejection shouldEqual MalformedRequestContentRejection("Malformed Input Json", DeserializationException("Malformed Input Json"))
      }
    }

    "return json when calling the ping route" in {
      Get("/ping") ~> routes ~> check {
        responseAs[String] shouldEqual "{\"hello\":\"world\",\"status\":\"up\"}"
      }
    }

    "leave POST request to other paths unhandled" in {
      Post("/somePath") ~> routes ~> check {
        handled shouldBe false
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/somePath") ~> routes ~> check {
        handled shouldBe false
      }
    }
  }
}
