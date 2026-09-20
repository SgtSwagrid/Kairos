package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*

/** Builders for the polls used throughout the solver's tests. */
object Fixture:

  /**
    * Whether these tests are running on the JVM rather than in a browser.
    *
    * The solver is cross-compiled, so its unit tests run on both and should:
    * they have caught nothing platform-specific yet, but they are cheap and
    * that is exactly the kind of fault they would catch. The end-to-end
    * simulations are a different matter. They spend minutes of arithmetic to
    * re-check behaviour the JVM run has already established, and on Scala.js
    * they are slow enough to exceed the test timeout outright.
    *
    * Scala.js prints a whole [[Double]] without its fractional part, where the
    * JVM keeps it. There is no tidier way to ask from shared source.
    */
  val onJvm: Boolean = 1.0.toString == "1.0"

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

  /** A number of ordinary participants, named in sequence. */
  def guests(count: Int, weight: Double = 1.0): List[Participant] = (1 to count)
    .map(index => participant(s"guest$index", weight))
    .toList

  /** The advice on a poll, with the irreducible floor measured and removed. */
  def advise(subject: Poll): Verdict =
    val belief = Belief.from(subject)
    Verdict.of(
      Ensemble.draw(belief, subject.bounds),
      Ensemble.draw(belief.saturated, subject.bounds),
    )

  /** The same answer from every participant about one slot. */
  def unanimous
    (
      people: Seq[Participant],
      slot: Slot,
      availability: Availability,
    )
    : List[Response] = people
    .map(person => says(person.name, slot, availability))
    .toList

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
