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
import sttp.model.StatusCode

/**
  * The implementation of the endpoints specified in [[PollApi]].
  *
  * @param polls
  *   The store in which polls are kept.
  */
final class PollService(polls: Polls) extends Service("polls"):

  import Service.Endpoint

  /** The default number of questions a round may contain. */
  private val Budget: Int = 40

  /** The default number of questions to put to any one participant. */
  private val Each: Int = 2

  /** The greatest number of questions a round may be asked for. */
  private val MostQuestions: Int = 500

  /** The greatest number of questions any one participant may be asked for. */
  private val MostEach: Int = 20

  /** Creates a poll from the organiser's description of it. */
  lazy val create: Endpoint = PollApi.create.serverLogic(started)

  /** Creates a worked example poll. */
  lazy val example: Endpoint = PollApi
    .example
    .serverLogic(_ => started(Example.draft))

  /** Lists every poll on the server. */
  lazy val list: Endpoint = PollApi
    .list
    .serverLogic(_ => polls.all.map(Right(_)))

  /** Reads a poll along with the solver's advice on it. */
  lazy val read: Endpoint = PollApi
    .read
    .serverLogic: (id, budget, each) =>
      onPoll(id)(report(_, budget, each))

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
      amend(id)(_.copy(objective = objective.bounded))

  /** Deletes a poll. */
  lazy val discard: Endpoint = PollApi
    .discard
    .serverLogic: id =>
      polls.discard(Id(id)).map(Either.cond(_, (), PollApi.missing))

  /**
    * Sends out the next round of questions.
    *
    * The round is worked out before the poll is touched, because solving is far
    * too slow to hold a lock over, and then recorded by amending the poll
    * rather than replacing it. Replacing it was a way of losing answers: the
    * poll would be written back as it stood before a solve lasting seconds,
    * discarding whatever had arrived in between. Recording questions changes no
    * answer, so the advice just derived still holds and is returned as it
    * stands.
    */
  lazy val send: Endpoint = PollApi
    .send
    .serverLogic: (id, budget, each) =>
      polls
        .get(Id(id))
        .flatMap:
          case None       => Left(PollApi.missing).pure[IO]
          case Some(poll) => analyse(poll, budget, each).flatMap: advice =>
              val questions = advice
                .round
                .enquiries
                .map(enquiry => Pending(enquiry.participant, enquiry.question))
              // Amended rather than replaced. Solving takes seconds, and writing
              // back the whole poll as it stood before would discard every
              // answer that arrived meanwhile; only the questions need
              // recording, and that is cheap enough to retry.
              polls
                .update(Id(id))(_.send(questions))
                .map(_.toRight(PollApi.missing).map(Report(_, advice)))

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
        .get(Id(id))
        .flatMap: existing =>
          // Checked before anything is written. Recording first and validating
          // afterwards meant that answers attributed to somebody who does not
          // exist, or to questions never put to them, were saved to disk and
          // only then rejected.
          admissible(existing, who, answers) match
            case Left(refusal)   => refusal.asLeft.pure[IO]
            case Right(recorded) => polls
                .update(Id(id))(_.record(recorded))
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
    * The answers that may be recorded, or why they may not be.
    *
    * A participant may answer only what was actually put to them, or revise
    * something they have answered before. Without that check an arbitrary
    * question could be attached to any answer, which both lets a guest sway the
    * result with questions nobody asked and lets the file grow without limit,
    * since a fresh question is never superseded.
    *
    * @param poll
    *   The poll being answered, if it exists.
    *
    * @param who
    *   The participant answering.
    *
    * @param answers
    *   What they have said.
    *
    * @return
    *   The responses to record, or the status to refuse with.
    */
  private def admissible
    (
      poll: Option[Poll],
      who: Id[Participant],
      answers: List[Answer],
    )
    : Either[StatusCode, List[Response]] =
    for
      subject <- poll.toRight(PollApi.missing)
      _       <- subject.participantsById.get(who).toRight(PollApi.missing)
      outstanding = subject.pendingBy(who).map(_.question).toSet ++
        subject.responsesBy(who).map(_.question)
      admitted = answers.filter(answer => outstanding(answer.question))
      _ <- Either.cond(
        admitted.sizeIs == answers.size,
        (),
        StatusCode.BadRequest,
      )
    yield admitted.map(answer =>
      Response(
        who,
        answer.question,
        answer.availability,
      ),
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
    : IO[Either[StatusCode, Report]] = polls
    .update(Id(id))(amendment)
    .flatMap(_.toRight(PollApi.missing).traverse(report(_, None, None)))

  /**
    * Reads a poll and derives something from it, or reports it missing.
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
  private def onPoll[A]
    (id: String)
    (derive: Poll => IO[A])
    : IO[Either[StatusCode, A]] = polls
    .get(Id(id))
    .flatMap(_.toRight(PollApi.missing).traverse(derive))

  /**
    * Starts a new poll from a draft and reports on it.
    *
    * @param draft
    *   The organiser's description of the poll.
    *
    * @return
    *   A report on the new poll.
    */
  private def started(draft: Draft): IO[Either[StatusCode, Report]] =
    IO(Id[Poll](UUID.randomUUID().toString.take(8)))
      .flatMap(id => polls.put(draft.toPoll(id)))
      .flatMap(report(_, None, None))
      .map(Right(_))

  /** A poll together with the solver's advice on it. */
  private def report
    (
      poll: Poll,
      budget: Option[Int],
      each: Option[Int],
    )
    : IO[Report] = analyse(poll, budget, each).map(Report(poll, _))

  /**
    * The solver's advice on a poll, at the requested round size.
    *
    * Run away from the pool that serves requests. Solving is seconds of
    * uninterrupted arithmetic, and on the compute pool a few of them at once
    * would starve everything else the server does, down to answering whether it
    * is alive.
    */
  private def analyse
    (
      poll: Poll,
      budget: Option[Int],
      each: Option[Int],
    )
    : IO[Analysis] = IO.blocking(Analysis.of(
    poll,
    // Bounded, because both govern how long the solver runs and both arrive
    // in a query string. A round of a million questions is one request that
    // would occupy a core for the rest of the afternoon.
    budget = budget.fold(Budget)(_.max(1).min(MostQuestions)),
    perParticipant = each.fold(Each)(_.max(1).min(MostEach)),
  ))

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
    : Either[StatusCode, Questionnaire] =
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
