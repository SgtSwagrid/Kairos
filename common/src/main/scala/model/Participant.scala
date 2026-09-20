package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * Someone whose attendance is wanted and whose availability is unknown.
  *
  * @param id
  *   The unique identifier of this participant.
  *
  * @param name
  *   The participant's name, for display to the organiser.
  *
  * @param weight
  *   How much this participant's attendance is worth relative to an ordinary
  *   participant at `1.0`. Raising the weight of a few essential people makes
  *   the solver both prefer slots they can attend and spend its questions on
  *   them first, which is usually what an organiser wants.
  *
  * @param group
  *   An optional label shared by participants who travel together, such as a
  *   household. Recorded for the organiser's benefit; the solver treats
  *   participants as independent.
  */
final case class Participant
  (
    id: Id[Participant],
    name: String,
    weight: Double = 1.0,
    group: Option[String] = None,
  )
  derives Codec.AsObject
