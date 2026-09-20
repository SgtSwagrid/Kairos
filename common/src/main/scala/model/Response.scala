package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * One participant's graded answer to one question.
  *
  * @param participant
  *   The participant who answered.
  *
  * @param question
  *   The question that was asked.
  *
  * @param availability
  *   The answer that was given.
  */
final case class Response
  (
    participant: Id[Participant],
    question: Question,
    availability: Availability,
  )
  derives Codec.AsObject
