package com.alecdorrington.common
package api

import com.alecdorrington.common.model.*
import io.circe.Codec

/**
  * A venue as the organiser describes it: a place, the stretches of time it is
  * free, and what it can hold. Candidate slots are derived from this rather
  * than entered by hand, since a venue free for a month offers many placements
  * and listing them individually would be tedious and error-prone.
  *
  * @param name
  *   The name of the venue.
  *
  * @param openings
  *   The stretches of time during which the venue is free.
  *
  * @param capacity
  *   The greatest number of participants the venue can hold.
  *
  * @param cost
  *   The cost of booking the venue for one placement.
  */
final case class VenueDraft
  (
    name: String,
    openings: List[Window],
    capacity: Int,
    cost: Double,
  )
  derives Codec.AsObject

/**
  * A participant as the organiser describes them.
  *
  * @param name
  *   The participant's name.
  *
  * @param weight
  *   How much their attendance is worth relative to an ordinary participant.
  */
final case class ParticipantDraft(name: String, weight: Double = 1.0)
  derives Codec.AsObject

/**
  * A poll as the organiser describes it, before slots have been worked out.
  *
  * @param title
  *   A description of the event being scheduled.
  *
  * @param venues
  *   The venues that could host the event.
  *
  * @param participants
  *   The people whose availability is to be elicited.
  *
  * @param length
  *   The number of days the event occupies.
  *
  * @param stride
  *   The spacing in days between candidate placements within an opening, so
  *   that `7` considers the same weekday each week.
  *
  * @param objective
  *   What the organiser is trying to maximise.
  */
final case class Draft
  (
    title: String,
    venues: List[VenueDraft],
    participants: List[ParticipantDraft],
    length: Int = 3,
    stride: Int = 7,
    objective: Objective = Objective.default,
  )
  derives Codec.AsObject:

  /**
    * Works this draft up into a poll, enumerating each venue's candidate
    * placements and assigning identifiers.
    *
    * @param id
    *   The identifier to give the new poll.
    *
    * @return
    *   The corresponding poll, with no answers yet recorded.
    */
  def toPoll(id: Id[Poll]): Poll = Poll(
    id = id,
    title = title,
    slots = venues.flatMap: venue =>
      Slot.enumerate(
        venue = venue.name,
        openings = venue.openings,
        capacity = venue.capacity,
        cost = venue.cost,
        length = length,
        stride = stride,
      ),
    participants = participants
      .zipWithIndex
      .map: (participant, index) =>
        Participant(
          id = Id(s"p$index"),
          name = participant.name,
          weight = participant.weight,
        ),
    objective = objective,
  )
