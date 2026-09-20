package com.alecdorrington.server
package services

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.common.api.*
import com.alecdorrington.common.model.*
import com.alecdorrington.common.solve.Analysis
import com.alecdorrington.server.api.PollApi
import com.alecdorrington.server.store.Polls
import java.util.UUID

/**
  * The implementation of the endpoints specified in [[PollApi]].
  *
  * @param polls
  *   The store in which polls are kept.
  */
final class PollService(polls: Polls) extends Service("polls"):

  import Service.Endpoint

  /** The default number of questions a round may contain. */
  private val Budget = 40

  /** The default number of questions to put to any one participant. */
  private val Each = 2

  /** Creates a poll from the organiser's description of it. */
  lazy val create: Endpoint = PollApi
    .create
    .serverLogic: draft =>
      for
        id   <- IO(Id[Poll](UUID.randomUUID().toString.take(8)))
        poll <- polls.put(draft.toPoll(id))
      yield Right(report(poll, None, None))

  /** Creates a worked example poll. */
  lazy val example: Endpoint = PollApi
    .example
    .serverLogic: _ =>
      for
        id   <- IO(Id[Poll](UUID.randomUUID().toString.take(8)))
        poll <- polls.put(Example.draft.toPoll(id))
      yield Right(report(poll, None, None))

  /** Lists every poll on the server. */
  lazy val list: Endpoint = PollApi
    .list
    .serverLogic(_ => polls.all.map(Right(_)))

  /** Reads a poll along with the solver's advice on it. */
  lazy val read: Endpoint = PollApi
    .read
    .serverLogic: (id, budget, each) =>
      found(id)(poll => report(poll, budget, each))

  /** Adds participants to an existing poll. */
  lazy val invite: Endpoint = PollApi
    .invite
    .serverLogic: (id, drafts) =>
      amend(id): poll =>
        val added = drafts
          .zipWithIndex
          .map: (draft, index) =>
            Participant(
              id = Id(s"p${ poll.participants.size + index }"),
              name = draft.name,
              weight = draft.weight,
            )
        poll.copy(participants = poll.participants ++ added)

  /** Revises what the organiser is trying to maximise. */
  lazy val retarget: Endpoint = PollApi
    .retarget
    .serverLogic: (id, objective) =>
      amend(id)(_.copy(objective = objective))

  /** Deletes a poll. */
  lazy val discard: Endpoint = PollApi
    .discard
    .serverLogic: id =>
      polls.discard(Id(id)).map(Either.cond(_, (), PollApi.missing))

  /**
    * Sends out the next round of questions.
    *
    * The round is worked out before the poll is touched rather than inside the
    * update, both because solving is far too slow to hold a lock over and
    * because an atomic update may be retried, and this one would be retried at
    * the cost of another full solve. Recording the questions does not change
    * any answer, so the advice just derived still holds and is returned as it
    * stands.
    */
  lazy val send: Endpoint = PollApi
    .send
    .serverLogic: (id, budget, each) =>
      polls
        .get(Id(id))
        .flatMap:
          case None       => Left(PollApi.missing).pure[IO]
          case Some(poll) =>
            val advice    = analyse(poll, budget, each)
            val questions = advice
              .round
              .enquiries
              .map(enquiry => Pending(enquiry.participant, enquiry.question))
            polls
              .put(poll.send(questions))
              .map(sent => Right(Report(sent, advice)))

  /** Fetches the questions awaiting one participant. */
  lazy val ask: Endpoint = PollApi
    .ask
    .serverLogic: (id, participant) =>
      polls.get(Id(id)).map(questionnaire(_, Id(participant)))

  /** Records one participant's answers. */
  lazy val answer: Endpoint = PollApi
    .answer
    .serverLogic: (id, participant, answers) =>
      val who = Id[Participant](participant)
      polls
        .update(Id(id)): poll =>
          poll.record(
            answers.map: answer =>
              Response(
                who,
                answer.question,
                answer.availability,
              ),
          )
        .map(questionnaire(_, who))

  override lazy val api: List[Endpoint] = List(
    create,
    example,
    list,
    read,
    invite,
    retarget,
    discard,
    send,
    ask,
    answer,
  )

  /**
    * Applies an amendment to a poll and reports the result.
    *
    * @param id
    *   The identifier of the poll to amend.
    *
    * @param amendment
    *   How to amend it.
    *
    * @return
    *   A report on the amended poll, or a not-found status.
    */
  private def amend
    (id: String)
    (amendment: Poll => Poll)
    : IO[Either[sttp.model.StatusCode, Report]] = polls
    .update(Id(id))(amendment)
    .map(_.toRight(PollApi.missing).map(report(_, None, None)))

  /**
    * Reads a poll and derives something from it.
    *
    * @param id
    *   The identifier of the poll to read.
    *
    * @param derive
    *   What to derive from it.
    *
    * @return
    *   The derived value, or a not-found status.
    */
  private def found[A]
    (id: String)
    (derive: Poll => A)
    : IO[Either[sttp.model.StatusCode, A]] = polls
    .get(Id(id))
    .map(_.toRight(PollApi.missing).map(derive))

  /** A poll together with the solver's advice on it. */
  private def report
    (
      poll: Poll,
      budget: Option[Int],
      each: Option[Int],
    )
    : Report = Report(poll, analyse(poll, budget, each))

  /** The solver's advice on a poll, at the requested round size. */
  private def analyse
    (
      poll: Poll,
      budget: Option[Int],
      each: Option[Int],
    )
    : Analysis = Analysis.of(
    poll,
    budget = budget.filter(_ > 0).getOrElse(Budget),
    perParticipant = each.filter(_ > 0).getOrElse(Each),
  )

  /**
    * The questions awaiting a participant, drawn from what was actually sent
    * rather than from what the solver would choose now.
    *
    * @param poll
    *   The poll being answered, if it exists.
    *
    * @param who
    *   The participant answering.
    *
    * @return
    *   Their questionnaire, or a not-found status.
    */
  private def questionnaire
    (poll: Option[Poll], who: Id[Participant])
    : Either[sttp.model.StatusCode, Questionnaire] =
    for
      subject     <- poll.toRight(PollApi.missing)
      participant <- subject.participantsById.get(who).toRight(PollApi.missing)
    yield Questionnaire(
      poll = subject.id,
      participant = who,
      name = participant.name,
      title = subject.title,
      // Outstanding questions first, then whatever they have answered before, so
      // that a participant can revise an earlier answer rather than being stuck
      // with it. Sending a question twice would be worse than useless.
      questions =
        (subject.pendingBy(who).map(_.question) ++
          subject.responsesBy(who).map(_.question))
          .distinct
          .map(question =>
            Asked(
              question,
              question.prompt(subject.slotsById, subject.length),
            ),
          ),
      answered = subject
        .responsesBy(who)
        .map(response =>
          Answer(
            response.question,
            response.availability,
          ),
        ),
    )
