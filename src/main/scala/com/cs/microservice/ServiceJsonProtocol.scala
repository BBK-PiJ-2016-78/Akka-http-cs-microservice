package com.cs.microservice

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

import scala.collection.immutable.ListMap

/**
  * Created by hradev01.
  */

class CreditCard(val cardName: String, val url: String, val apr: Float, var eligibility: Float,
                 val features: Option[List[String]], var cardScore: Option[Double]) {

  def this(cardName: String, url: String, apr: Float, eligibility: Float, features: Option[List[String]]) = {
    this(cardName, url, apr, eligibility, features, None)
  }

  def roundNumber(x: Double): Double = {
    BigDecimal(x).setScale(2, BigDecimal.RoundingMode.UP).toDouble
  }

}

class FormInput(val firstName: String, val lastName: String, val dateOfBirth: String, val creditScore: Int,
                val employmentStatus: Option[String], val salary: Option[Int]) {

  var fullName: Option[String] = None

  def this(firstName: String, lastName: String, dateOfBirth: String, creditScore: Int) {
    this(firstName, lastName, dateOfBirth, creditScore, None, None)
    this.fullName = Some(firstName + " " + lastName)
  }

  def changeFullName(clear: Boolean): Unit = if(clear) this.fullName = None else this.fullName = Some(firstName + " " + lastName)

}

/**
  * This object is used for defining the Marshalling and Unmarshalling of objects to Json and back.
  * The CreditCard object can read the two types of responses that come from the different cards.
  * The FormInput object can write into different formats depending if the fullName is set or not.
  */
object ServiceJsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit object FormInputJsonFormat extends RootJsonFormat[FormInput] {
    override def write(in: FormInput): JsValue =
      in.fullName match {
        case Some(value) => JsObject(ListMap("fullName" -> JsString(value), "dateOfBirth" -> JsString(in.dateOfBirth),
          "creditScore" -> JsNumber(in.creditScore)))
        case None if in.employmentStatus.isDefined && in.salary.isDefined =>
          JsObject(ListMap("first-name" -> JsString(in.firstName), "last-name" -> JsString(in.lastName),
            "date-of-birth" -> JsString(in.dateOfBirth), "score" -> JsNumber(in.creditScore),
            "employment-status" -> JsString(in.employmentStatus.get), "salary" -> JsNumber(in.salary.get)))
        case _ => JsObject()
      }

    override def read(value: JsValue): FormInput =
      value.asJsObject.getFields("firstName", "lastName", "dateOfBirth", "creditScore", "employmentStatus", "salary")
      match {
      case Seq(JsString(firstName), JsString(lastName), JsString(dob), JsNumber(score)) =>
        new FormInput(firstName, lastName, dob, score.intValue())
      case Seq(JsString(firstName), JsString(lastName), JsString(dob), JsNumber(score),
            JsString(empStatus), JsNumber(salary)) =>
        new FormInput(firstName, lastName, dob, score.intValue(), Some(empStatus), Some(salary.intValue()))
      case _ => throw DeserializationException("Malformed Input Json")
    }
  }
  implicit object CreditCardsJsonFormat extends RootJsonFormat[CreditCard] {
    override def read(json: JsValue): CreditCard =
      json.asJsObject.getFields("cardName", "card", "url", "apply-url", "apr", "annual-percentage-rate",
        "eligibility", "approval-rating", "features", "attributes", "introductory-offers") match {
        case Seq(JsString(card), JsString(url), JsNumber(apr), JsNumber(eligibility)) =>
          new CreditCard(card, url, apr.floatValue(), eligibility.floatValue(), None)
        case Seq(JsString(card), JsString(url), JsNumber(apr), JsNumber(eligibility), JsArray(features)) =>
          new CreditCard(card, url, apr.floatValue(), eligibility.floatValue(), Some(features.map(_.toString()).toList))
        case Seq(JsString(card), JsString(url), JsNumber(apr), JsNumber(eligibility), JsArray(attributes), JsArray(offers)) =>
          new CreditCard(card, url, apr.floatValue(), eligibility.floatValue(), Some(attributes.++(offers).map(_.toString()).toList))
        case _ => throw DeserializationException("Malformed Incoming Json")
      }

    override def write(in: CreditCard): JsValue =
      (in.features, in.cardScore) match {
        case (Some(value), None) =>
          JsObject(ListMap("cardName" -> JsString(in.cardName), "url" -> JsString(in.url),
            "apr" -> JsNumber(in.roundNumber(in.apr)), "eligibility" -> JsNumber(in.roundNumber(in.eligibility)),
            "features" -> JsArray(JsString(value.mkString(",")))))
        case (Some(value), Some(score)) =>
          JsObject(ListMap("cardName" -> JsString(in.cardName), "url" -> JsString(in.url),
            "apr" -> JsNumber(in.roundNumber(in.apr)), "eligibility" -> JsNumber(in.roundNumber(in.eligibility)),
            "features" -> JsArray(JsString(value.mkString(","))), "card-score" -> JsNumber(in.roundNumber(score))))
        case (None, Some(score)) =>
          JsObject(ListMap("cardName" -> JsString(in.cardName), "url" -> JsString(in.url),
            "apr" -> JsNumber(in.roundNumber(in.apr)), "eligibility" -> JsNumber(in.roundNumber(in.eligibility)),
            "card-score" -> JsNumber(in.roundNumber(score))))
        case (None, None) =>
          JsObject(ListMap("cardName" -> JsString(in.cardName), "url" -> JsString(in.url),
            "apr" -> JsNumber(in.roundNumber(in.apr)), "eligibility" -> JsNumber(in.roundNumber(in.eligibility))))
      }
  }
}
