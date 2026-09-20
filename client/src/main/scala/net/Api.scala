package com.alecdorrington.client
package net

import com.alecdorrington.common.api.*
import com.alecdorrington.common.model.*
import com.raquo.airstream.core.EventStream
import io.circe.{Decoder, Json}
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js

/**
  * The server's endpoints, as seen from the browser.
  *
  * Every call yields a stream that emits once and then ends, which is how
  * Laminar prefers to receive the result of a request: bind it to the DOM and
  * the subscription is torn down with the element that asked for it.
  */
object Api:

  /** Creates a worked example poll. */
  def example: EventStream[Report] = send[Report]("POST", "/api/polls/example")

  /** Creates a poll from the organiser's description of it. */
  def create(draft: Draft): EventStream[Report] = send[Report](
    "POST",
    "/api/polls",
    Some(draft.asJson),
  )

  /**
    * Reads a poll along with the advice on it.
    *
    * @param poll
    *   The identifier of the poll to read.
    *
    * @param budget
    *   How many questions the next round may contain.
    *
    * @param each
    *   How many questions to put to any one participant.
    *
    * @return
    *   A report on the poll.
    */
  def read(poll: String, budget: Int, each: Int): EventStream[Report] =
    send[Report](
      "GET",
      s"/api/polls/$poll?budget=$budget&each=$each",
    )

  /** Sends out the next round of questions. */
  def sendRound(poll: String, budget: Int, each: Int): EventStream[Report] =
    send[Report](
      "POST",
      s"/api/polls/$poll/round?budget=$budget&each=$each",
    )

  /** Adds participants to a poll. */
  def invite
    (
      poll: String,
      added: List[ParticipantDraft],
    )
    : EventStream[Report] = send[Report](
    "POST",
    s"/api/polls/$poll/participants",
    Some(added.asJson),
  )

  /** Revises what the organiser is trying to maximise. */
  def retarget(poll: String, objective: Objective): EventStream[Report] =
    send[Report](
      "PUT",
      s"/api/polls/$poll/objective",
      Some(objective.asJson),
    )

  /** Fetches the questions awaiting one participant. */
  def ask(poll: String, participant: String): EventStream[Questionnaire] =
    send[Questionnaire](
      "GET",
      s"/api/polls/$poll/ask/$participant",
    )

  /** Records one participant's answers. */
  def answer
    (
      poll: String,
      participant: String,
      answers: List[Answer],
    )
    : EventStream[Questionnaire] = send[Questionnaire](
    "POST",
    s"/api/polls/$poll/ask/$participant",
    Some(answers.asJson),
  )

  /**
    * Makes a request and decodes the response.
    *
    * @param method
    *   The HTTP method to use.
    *
    * @param url
    *   The address to request.
    *
    * @param body
    *   The JSON to send, if any.
    *
    * @return
    *   A stream emitting the decoded response, or failing with the reason it
    *   could not be obtained.
    */
  private def send[A : Decoder]
    (
      method: String,
      url: String,
      body: Option[Json] = None,
    )
    : EventStream[A] = EventStream.fromFuture(fetch(method, url, body))

  /** Performs the request, failing the future on anything other than success. */
  private def fetch[A : Decoder]
    (
      method: String,
      url: String,
      body: Option[Json],
    )
    : Future[A] =
    val options = js.Dynamic.literal(method = method)
    body.foreach: json =>
      options.body = json.noSpaces
      options.headers = js.Dynamic.literal("Content-Type" -> "application/json")

    dom
      .fetch(
        url,
        options.asInstanceOf[dom.RequestInit],
      )
      .toFuture
      .flatMap: response =>
        response
          .text()
          .toFuture
          .flatMap: text =>
            if !response.ok then
              Future.failed(Exception(s"$method $url: ${ response.status }"))
            else decode[A](text).fold(Future.failed, Future.successful)
