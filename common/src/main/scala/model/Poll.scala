package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * A complete scheduling problem: the slots that could be chosen, the people
  * whose attendance is wanted, and everything they have said so far.
  *
  * A poll is the whole persistent state of the process. Rounds of elicitation
  * are not stored separately; they are derived by asking the solver what is
  * still worth knowing, which means a poll can be resumed, amended or replayed
  * at any point without bookkeeping.
  *
  * @param id
  *   The unique identifier of this poll.
  *
  * @param title
  *   A description of the event being scheduled.
  *
  * @param slots
  *   The candidate slots, exactly one of which will eventually be chosen.
  *
  * @param participants
  *   The people whose availability is being elicited.
  *
  * @param responses
  *   Every answer received so far, across all rounds.
  *
  * @param objective
  *   What the organiser is trying to maximise.
  *
  * @param pending
  *   The questions that have been sent out and are awaiting answers.
  *
  * @param roundsSent
  *   How many rounds of questions have been sent out so far.
  */
final case class Poll
  (
    id: Id[Poll],
    title: String,
    slots: List[Slot],
    participants: List[Participant],
    responses: List[Response] = List.empty,
    objective: Objective = Objective.default,
    pending: List[Pending] = List.empty,
    roundsSent: Int = 0,
  )
  derives Codec.AsObject:

  /**
    * What the organiser is trying to maximise, held within the range each
    * setting is meaningful over. Everything in the solver reads the objective
    * through here rather than through the field, so a poll restored from a
    * hand-edited file cannot carry a setting that breaks it.
    */
  lazy val bounds: Objective = objective.bounded

  /** Every slot, indexed by identifier. */
  lazy val slotsById: Map[Id[Slot], Slot] = slots
    .map(slot => slot.id -> slot)
    .toMap

  /** Every participant, indexed by identifier. */
  lazy val participantsById: Map[Id[Participant], Participant] = participants
    .map(participant => participant.id -> participant)
    .toMap

  /** Every answer, grouped by the participant who gave it. */
  lazy val responsesBy: Map[Id[Participant], List[Response]] = responses
    .groupBy(_.participant)
    .withDefaultValue(List.empty)

  /**
    * The typical number of days a slot occupies, used to phrase pooled
    * questions in terms of a commitment of the right size.
    */
  lazy val length: Int =
    if slots.isEmpty then 1 else slots.map(_.window.length).sum / slots.size

  /** The participants who have answered nothing at all. */
  lazy val silent: List[Participant] =
    participants.filter(participant => responsesBy(participant.id).isEmpty)

  /** The questions awaiting an answer, grouped by who was asked. */
  lazy val pendingBy: Map[Id[Participant], List[Pending]] = pending
    .groupBy(_.participant)
    .withDefaultValue(List.empty)

  /**
    * Sends out a round of questions, replacing any still outstanding from the
    * round before. A participant who never replied is not owed two rounds of
    * questions at once; the solver will ask again for whatever it still wants.
    *
    * @param questions
    *   The questions to send.
    *
    * @return
    *   This poll, with those questions outstanding and the round counted.
    */
  def send(questions: Seq[Pending]): Poll = copy(
    pending = questions.toList,
    roundsSent = roundsSent + 1,
  )

  /**
    * Records further answers, replacing any earlier answer to the same question
    * by the same participant so that participants may freely revise what they
    * have said.
    *
    * @param further
    *   The answers to record.
    *
    * @return
    *   This poll, updated with the given answers.
    */
  def record(further: Seq[Response]): Poll =
    val superseded = further
      .map(reply => reply.participant -> reply.question)
      .toSet
    copy(
      responses = responses.filterNot: reply =>
        superseded(reply.participant -> reply.question)
      ++ further,
      pending = pending.filterNot: question =>
        superseded(question.participant -> question.question),
    )
