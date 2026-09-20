package com.alecdorrington.server
package api

import com.alecdorrington.common.api.*
import com.alecdorrington.common.model.*
import com.alecdorrington.server.api.Schemas.given
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.circe.*

/** The endpoints by which polls are created, read, and answered. */
object PollApi:

  /**
    * An endpoint of this API: it takes an input, and either fails with a status
    * or returns an output. Named so that the endpoints below can carry their
    * types, which the spelt-out form is too unwieldy to allow.
    *
    * @tparam In
    *   What the request carries.
    *
    * @tparam Out
    *   What the response carries.
    */
  type Route[In, Out] = PublicEndpoint[In, StatusCode, Out, Any]

  /** The identifier of the poll being acted upon. */
  private val poll = path[String]("poll").description("The poll's identifier.")

  /** The identifier of the participant answering. */
  private val participant = path[String]("participant").description(
    "The answering participant's identifier.",
  )

  /** The base path shared by every endpoint here. */
  private val polls = endpoint
    .in("api" / "polls")
    .errorOut(statusCode)
    .tag("polls")

  /** Creates a poll from the organiser's description of it. */
  val create: Route[Draft, Report] = polls
    .post
    .in(jsonBody[Draft])
    .out(jsonBody[Report])
    .summary("Create a poll.")

  /** Creates a worked example, so the tool can be tried without setting one up. */
  val example: Route[Unit, Report] = polls
    .post
    .in("example")
    .out(jsonBody[Report])
    .summary("Create a worked example poll.")

  /**
    * Reads a poll along with the solver's advice on it. The size of the next
    * round is a parameter of the request rather than of the poll, so that an
    * organiser can weigh a larger round against a smaller one before sending
    * either.
    */
  val read
    : Route[
      (String, Option[Int], Option[Int]),
      Report,
    ] = polls
    .get
    .in(poll)
    .in(query[Option[Int]]("budget").description("Questions per round."))
    .in(query[Option[Int]]("each").description("Questions per participant."))
    .out(jsonBody[Report])
    .summary("Read a poll and the advice on it.")

  /** Adds participants to an existing poll. */
  val invite
    : Route[
      (String, List[ParticipantDraft]),
      Report,
    ] = polls
    .post
    .in(poll)
    .in("participants")
    .in(jsonBody[List[ParticipantDraft]])
    .out(jsonBody[Report])
    .summary("Add participants to a poll.")

  /** Revises what the organiser is trying to maximise. */
  val retarget: Route[(String, Objective), Report] = polls
    .put
    .in(poll)
    .in("objective")
    .in(jsonBody[Objective])
    .out(jsonBody[Report])
    .summary("Revise a poll's objective.")

  /** Deletes a poll and everything said in it. */
  val discard: Route[String, Unit] = polls
    .delete
    .in(poll)
    .out(emptyOutput)
    .summary("Delete a poll.")

  /**
    * Sends out the next round of questions, recording them against the poll so
    * that each participant is asked what was actually sent to them.
    */
  val send
    : Route[
      (String, Option[Int], Option[Int]),
      Report,
    ] = polls
    .post
    .in(poll)
    .in("round")
    .in(query[Option[Int]]("budget").description("Questions per round."))
    .in(query[Option[Int]]("each").description("Questions per participant."))
    .out(jsonBody[Report])
    .summary("Send out the next round of questions.")

  /** Fetches the questions awaiting one participant. */
  val ask: Route[(String, String), Questionnaire] = polls
    .get
    .in(poll)
    .in("ask" / participant)
    .out(jsonBody[Questionnaire])
    .summary("Fetch a participant's questions.")

  /** Records one participant's answers, and returns whatever is left to ask. */
  val answer
    : Route[
      (String, String, List[Answer]),
      Questionnaire,
    ] = polls
    .post
    .in(poll)
    .in("ask" / participant)
    .in(jsonBody[List[Answer]])
    .out(jsonBody[Questionnaire])
    .summary("Record a participant's answers.")

  /** The status code to return when a poll or participant is not found. */
  val missing: StatusCode = StatusCode.NotFound
