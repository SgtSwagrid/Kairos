package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * A question that has been put to a participant and is awaiting an answer.
  *
  * Committing a round to the poll, rather than working out afresh what each
  * participant should be asked when they happen to open their link, matters for
  * two reasons. A participant must see the question that was actually sent to
  * them, not whatever the solver would choose today in light of what others
  * have since said; and answering should not require solving the poll again, so
  * that a link opens at once however many people are still to reply.
  *
  * @param participant
  *   The participant who was asked.
  *
  * @param question
  *   The question they were asked.
  */
final case class Pending
  (
    participant: Id[Participant],
    question: Question,
  )
  derives Codec.AsObject
