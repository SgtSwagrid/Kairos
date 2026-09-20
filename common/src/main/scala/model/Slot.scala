package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * One candidate way to hold the event: a named venue, held for a specific
  * window, with a fixed capacity and cost. Slots are the alternatives between
  * which the organiser must ultimately choose, and the only thing participants
  * are ever asked about.
  *
  * Modelling a venue's several openings as several slots keeps the choice flat:
  * a venue that is free on two weekends contributes two independent candidates,
  * which may be ranked differently because the guests available differ.
  *
  * @param id
  *   The unique identifier of this slot.
  *
  * @param venue
  *   The name of the venue, which several slots may share.
  *
  * @param window
  *   The days the event would occupy.
  *
  * @param capacity
  *   The greatest number of participants the venue can hold.
  *
  * @param cost
  *   The cost of booking this slot, in whatever unit the organiser prefers.
  */
final case class Slot
  (
    id: Id[Slot],
    venue: String,
    window: Window,
    capacity: Int,
    cost: Double,
  )
  derives Codec.AsObject:

  /** This slot rendered for display, naming both the venue and the dates. */
  def show: String = s"$venue, ${ window.showBrief }"

object Slot:

  /**
    * Enumerates the candidate slots offered by a venue, by sliding an event of
    * fixed length across each of the venue's openings.
    *
    * This is the step that keeps elicitation tractable: participants are never
    * asked about arbitrary days, only about placements the venue can actually
    * host, which is typically a few dozen candidates rather than a year.
    *
    * @param venue
    *   The name of the venue.
    *
    * @param openings
    *   The windows during which the venue is free, each of which may host
    *   several placements.
    *
    * @param capacity
    *   The greatest number of participants the venue can hold.
    *
    * @param cost
    *   The cost of booking the venue for one placement.
    *
    * @param length
    *   The number of days the event occupies.
    *
    * @param stride
    *   The spacing in days between successive placements, so that a stride of
    *   `7` considers the same weekday each week.
    *
    * @return
    *   Every placement of the event that fits wholly within an opening.
    */
  def enumerate
    (
      venue: String,
      openings: Seq[Window],
      capacity: Int,
      cost: Double,
      length: Int,
      stride: Int = 7,
    )
    : List[Slot] =
    for
      opening <- openings.toList
      offset  <- 0.to(opening.length - length).by(math.max(1, stride))
      window = Window.of(opening.start.plus(offset), length)
    yield Slot(
      id = Id(s"$venue@${ window.start.iso }"),
      venue = venue,
      window = window,
      capacity = capacity,
      cost = cost,
    )
