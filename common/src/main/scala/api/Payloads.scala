package com.alecdorrington.common
package api

import com.alecdorrington.common.model.*
import com.alecdorrington.common.solve.Analysis
import io.circe.Codec

/**
  * Everything the organiser's page needs: the poll as it stands and the
  * solver's reading of it.
  *
  * @param poll
  *   The poll, including every answer received so far.
  *
  * @param analysis
  *   What to choose, how sure that is, and what to ask next.
  */
final case class Report(poll: Poll, analysis: Analysis) derives Codec.AsObject

/**
  * One question as it should be put to a participant.
  *
  * @param question
  *   The question, to be echoed back with the answer.
  *
  * @param prompt
  *   The question as the participant should read it.
  */
final case class Asked(question: Question, prompt: String)
  derives Codec.AsObject

/**
  * The questions awaiting one participant.
  *
  * This deliberately carries no part of the poll beyond what the participant is
  * being asked. Participants have no business seeing the guest list, how
  * heavily anyone is weighted, or which dates are winning, and the surest way
  * to keep that so is for the server never to send it.
  *
  * @param poll
  *   The poll being answered.
  *
  * @param participant
  *   The participant being asked.
  *
  * @param name
  *   The participant's name, so they can check they have the right link.
  *
  * @param title
  *   A description of the event being scheduled.
  *
  * @param questions
  *   The questions to put to them, which may be empty if nothing is needed.
  *
  * @param answered
  *   The questions they have already answered, so that they may revise them.
  */
final case class Questionnaire
  (
    poll: Id[Poll],
    participant: Id[Participant],
    name: String,
    title: String,
    questions: List[Asked],
    answered: List[Answer],
  )
  derives Codec.AsObject

/**
  * One participant's answer to one question.
  *
  * @param question
  *   The question being answered.
  *
  * @param availability
  *   The grade given in answer.
  */
final case class Answer
  (
    question: Question,
    availability: Availability,
  )
  derives Codec.AsObject

/**
  * A summary of one participant, for the organiser's overview.
  *
  * @param participant
  *   The participant summarised.
  *
  * @param answered
  *   How many questions they have answered.
  *
  * @param pending
  *   How many questions the next round would put to them.
  *
  * @param best
  *   Their probability of attending the recommended slot.
  */
final case class Standing
  (
    participant: Participant,
    answered: Int,
    pending: Int,
    best: Double,
  )
  derives Codec.AsObject
