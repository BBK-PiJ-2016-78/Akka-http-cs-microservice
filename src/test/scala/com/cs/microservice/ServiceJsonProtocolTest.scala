package com.cs.microservice

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import ServiceJsonProtocol._
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by hradev01 on 1/14/2018.
  */
class ServiceJsonProtocolTest extends WordSpec with Matchers with ScalatestRouteTest {

  val jsonServiceRoute: Route =
    post {
      path("jsonInputTest") {
        entity(as[FormInput]) { in =>
          complete(in)
        }
      }
    } ~
      post {
        path("creditCardsTest") {
          entity(as[CreditCard]) { in =>
            complete(in)
          }
        }
      }

  "The service" should {

    "return csCards friendly json output preserving the order, when 4 required fields are provided" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\", \"creditScore\": 500}")
      Post("/jsonInputTest", httpEntity) ~> jsonServiceRoute ~> check {
        responseAs[String] shouldEqual "{\"fullName\":\"John Smith\",\"dateOfBirth\":\"1990/01/23\",\"creditScore\":500}"
      }
    }

    "return scoreCards friendly json output preserving the order, when all required fields are provided" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\", \"creditScore\": 500, \"employmentStatus\": \"full-time\", \"salary\": 25000}")
      Post("/jsonInputTest", httpEntity) ~> jsonServiceRoute ~> check {
        responseAs[String] shouldEqual "{\"first-name\":\"John\",\"last-name\":\"Smith\",\"date-of-birth\":\"1990/01/23\",\"score\":500,\"employment-status\":\"full-time\",\"salary\":25000}"
      }
    }

    "ensure that the request method was POST " in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\", \"creditScore\": 500, \"employmentStatus\": \"full-time\", \"salary\": 25000}")
      Post("/jsonInputTest", httpEntity) ~> {extract(_.request.method.value){complete(_)}} ~> check {
        responseAs[String] shouldEqual "POST"
      }
    }

    "ensure that the response does not equal the format of the request" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"dateOfBirth\": \"1990/01/23\", \"creditScore\": 500, \"employmentStatus\": \"full-time\", \"salary\": 25000}")
      Post("/jsonInputTest", httpEntity) ~> {extract(_.request.method.value){complete(_)}} ~> check {
        responseEntity should not equal httpEntity
      }
    }

    "return the credit card input without card-score json as a response" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\"cardName\": \"SuperSaver Card\", \"url\": \"http://www.example.com/apply\", \"apr\": 21.4, \"eligibility\": 6.3}")
      Post("/creditCardsTest", httpEntity) ~> jsonServiceRoute ~> check {
        responseAs[String] shouldEqual "{\"cardName\":\"SuperSaver Card\",\"url\":\"http://www.example.com/apply\",\"apr\":21.4,\"eligibility\":6.31}"
      }
    }

    "return the credit card input with card-score json as a response" in {
      val httpEntity = HttpEntity(ContentTypes.`application/json`, "{\n        \"cardName\": \"ScoredCard Builder\",\n        \"url\": \"http://www.example.com/apply\",\n        \"apr\": 19.4,\n        \"eligibility\": 8,\n        \"features\": [\n            \"\\\"Supports ApplePay\\\",\\\"Interest free purchases for 1 month\\\"\"\n        ],\n        \"card-score\": 0.03\n    }")
      Post("/creditCardsTest", httpEntity) ~> jsonServiceRoute ~> check {
        responseAs[String] shouldEqual "{\"cardName\":\"ScoredCard Builder\",\"url\":\"http://www.example.com/apply\",\"apr\":19.4,\"eligibility\":8.0,\"features\":[\"\\\"\\\\\\\"Supports ApplePay\\\\\\\",\\\\\\\"Interest free purchases for 1 month\\\\\\\"\\\"\"]}"
      }
    }

  }
}
