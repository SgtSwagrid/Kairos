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
    * The given slots with any repeated identifier made distinct.
    *
    * An identifier is built from a venue's name and a start date, neither of
    * which is guaranteed unique: two venues may share a name, and one venue's
    * openings may overlap. A repeat is quietly destructive, because
    * [[Poll.slotsById]] keeps only the last of them while the poll still lists
    * both, so a question about that identifier becomes ambiguous and the maps
    * of confidence and regret lose an entry.
    *
    * @param slots
    *   The slots to make distinct, in order.
    *
    * @return
    *   The same slots in the same order, with later repeats numbered.
    */
  def distinct(slots: Seq[Slot]): List[Slot] = slots
    .foldLeft((List.empty[Slot], Map.empty[String, Int])):
      case ((kept, seen), slot) =>
        val taken = seen.getOrElse(slot.id.value, 0)
        val id    =
          if taken == 0 then slot.id else Id[Slot](s"${ slot.id.value }~$taken")
        (slot.copy(id = id) :: kept, seen.updated(slot.id.value, taken + 1))
    ._1
    .reverse

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
