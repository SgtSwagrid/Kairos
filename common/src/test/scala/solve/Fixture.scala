package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*

/** Builders for the polls used throughout the solver's tests. */
object Fixture:

  /**
    * A slot at the given venue, beginning on the given day.
    *
    * @param venue
    *   The name of the venue.
    *
    * @param start
    *   The first day of the slot, as an ISO date.
    *
    * @param length
    *   The number of days the slot occupies.
    *
    * @param capacity
    *   The greatest number of participants the venue can hold.
    *
    * @param cost
    *   The cost of booking the slot.
    *
    * @return
    *   The corresponding slot.
    */
  def slot
    (
      venue: String,
      start: String,
      length: Int = 3,
      capacity: Int = 200,
      cost: Double = 0.0,
    )
    : Slot = Slot(
    id = Id(s"$venue@$start"),
    venue = venue,
    window = Window.of(Day.parse(start).get, length),
    capacity = capacity,
    cost = cost,
  )

  /** A participant of the given name and weight. */
  def participant(name: String, weight: Double = 1.0): Participant =
    Participant(id = Id(name), name = name, weight = weight)

  /** A poll over the given slots and participants, with nothing yet answered. */
  def poll
    (
      slots: List[Slot],
      participants: List[Participant],
      objective: Objective = Objective.default,
    )
    : Poll = Poll(
    id = Id("test"),
    title = "Test poll",
    slots = slots,
    participants = participants,
    objective = objective,
  )

  /** A named window spanning the given inclusive ISO dates. */
  def window(from: String, to: String): Window =
    Window(Day.parse(from).get, Day.parse(to).get)

  /** An answer from the named participant to a question about one slot. */
  def says
    (
      participant: String,
      slot: Slot,
      availability: Availability,
    )
    : Response = Response(
    Id(participant),
    Question.AboutSlot(slot.id),
    availability,
  )

  /** An answer from the named participant to a pooled question. */
  def saysOf
    (
      participant: String,
      window: Window,
      availability: Availability,
    )
    : Response = Response(
    Id(participant),
    Question.AboutWindow(window),
    availability,
  )
