package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*
import com.alecdorrington.common.solve.Fixture.*
import munit.FunSuite
import scala.util.Random

/**
  * Tests the solver end to end against participants whose availability is fixed
  * in advance and hidden from it, by conducting a few rounds of questions and
  * asking whether the slot it settles on is in fact a good one.
  *
  * This is the claim the whole design rests on: that a handful of rounds, each
  * asking each participant only one or two questions, suffices to identify a
  * near-best slot out of many. Everything else is only machinery.
  */
class SimulationSuite extends FunSuite:

  /** Two venues, each free on a series of weekends across the summer. */
  private val slots = Slot.enumerate(
    venue = "Alpine Hut",
    openings = List(window("2027-05-07", "2027-09-26")),
    capacity = 70,
    cost = 4000,
    length = 3,
    stride = 21,
  ) ++ Slot.enumerate(
    venue = "Lake Lodge",
    openings = List(window("2027-06-04", "2027-08-28")),
    capacity = 34,
    cost = 2500,
    length = 3,
    stride = 21,
  )

  /** Forty guests, four of whom must be there. */
  private val guests = (1 to 36)
    .map(index => participant(s"guest$index"))
    .toList ++ (1 to 4)
      .map(index => participant(s"family$index", weight = 8.0))
      .toList

  private val base = poll(slots, guests)

  /**
    * The hidden truth of who could attend what.
    *
    * Each guest favours one month, and is usually free on its weekends and
    * usually not on others, which is roughly how travel to the far side of the
    * world behaves: it is governed by a season rather than by single dates. A
    * tenth of the guests cannot come at all.
    *
    * @return
    *   Whether each guest could attend each slot, by guest and then slot.
    */
  private lazy val truth: Vector[Vector[Boolean]] =
    val random = Random(1987L)
    guests
      .toVector
      .map: _ =>
        // June is the popular month, so a clear favourite should emerge.
        val favoured = random.shuffle(List(5, 6, 6, 6, 7, 8)).head
        val willing  = random.nextDouble() > 0.1
        slots
          .toVector
          .map: candidate =>
            val chance =
              if candidate.window.start.month == favoured then 0.85 else 0.2
            willing && random.nextDouble() < chance

  /** The true value of a slot, were everybody's availability known exactly. */
  private def value(slot: Int): Double =
    val attending = guests.indices.filter(truth(_)(slot))
    val excess    = math.max(0, attending.size - slots(slot).capacity)
    attending.map(guests(_).weight).sum -
      base.objective.costWeight * slots(slot).cost -
      base.objective.overflowWeight * excess

  /** The answer a guest would truthfully give to a question. */
  private def reply(guest: Int, question: Question): Availability =
    val bearing = slots.indices.filter(slot => question.bearsOn(slots(slot)))
    if bearing.exists(truth(guest)) then Availability.Yes else Availability.No

  /** Conducts one round, returning the analysis and the poll it leads to. */
  private def conduct(current: Poll, budget: Int): (Analysis, Poll) =
    val analysis = Analysis.of(current, budget, perParticipant = 2)
    val answers  = analysis
      .round
      .enquiries
      .map: enquiry =>
        val guest = guests.indexWhere(_.id == enquiry.participant)
        Response(
          enquiry.participant,
          enquiry.question,
          reply(guest, enquiry.question),
        )
    (analysis, current.record(answers))

  test("three short rounds find a near-best slot out of many"):

    val rounds = 3
    val budget = 30

    val (history, finished) = (1 to rounds).foldLeft(
      (List.empty[Analysis], base),
    ):
      case ((log, current), _) =>
        val (analysis, next) = conduct(current, budget)
        (log :+ analysis, next)

    val outcome = Analysis.of(finished, budget, perParticipant = 2)
    val chosen  = slots.indexWhere(_.id == outcome.verdict.recommended.get.id)

    val values = slots.indices.map(value)
    val best   = values.max
    val worst  = values.min
    val middle = values.sum / values.size

    // Report the course of the process, so a failure is legible.
    println(f"\n  Slots: ${ slots.size }, guests: ${ guests.size }")
    println(f"  True value: best $best%.1f, mean $middle%.1f, worst $worst%.1f")
    history
      .zipWithIndex
      .foreach: (analysis, index) =>
        println(
          f"  Round ${ index + 1 }: ${ analysis
              .round
              .enquiries
              .size }%2d questions" +
            f" to ${ analysis.round.recipients }%2d guests," +
            f" value of information ${ analysis.verdict.information }%6.2f",
        )
    println(
      f"  Chose ${ slots(chosen).show }: true value ${ values(chosen) }%.1f" +
        f" (${ 100 * values(chosen) / best }%.0f%% of best)",
    )
    println(f"  Asked ${ finished.responses.size } questions of ${ guests
        .size } guests," + f" ${ finished.responses.size.toDouble /
          guests.size }%.1f each\n")

    assert(
      values(chosen) >= 0.95 * best,
      f"chose a slot worth ${ values(chosen) }%.1f against a best of $best%.1f",
    )

    assert(
      values(chosen) > middle + 0.6 * (best - middle),
      "the choice should be far better than an uninformed one",
    )

  test("asking narrows what remains to be learned"):
    // Compared start to finish rather than round by round. The value of
    // information is not monotone and should not be asserted to be: learning that
    // many guests are free in one month widens the gap between the options, so
    // what it would cost to choose wrongly can rise for a round before it falls.
    val (first, afterFirst) = conduct(base, 30)
    val (_, afterSecond)    = conduct(afterFirst, 30)
    val (_, afterThird)     = conduct(afterSecond, 30)
    val last                = Analysis.of(afterThird, 30, perParticipant = 2)

    assert(
      last.verdict.information < 0.8 * first.verdict.information,
      s"${ last.verdict.information } should be well below " +
        s"${ first.verdict.information }",
    )

  test("confidence in the leading slot grows as answers arrive"):
    val (first, afterFirst) = conduct(base, 30)
    val (_, afterSecond)    = conduct(afterFirst, 30)
    val outcome             = Analysis.of(afterSecond, 30)

    def leading(analysis: Analysis): Double =
      analysis.verdict.confidence.values.max

    assert(
      leading(outcome) > leading(first),
      s"${ leading(outcome) } should exceed ${ leading(first) }",
    )

  test("no guest is asked more than a handful of questions in total"):
    val (_, afterFirst)  = conduct(base, 30)
    val (_, afterSecond) = conduct(afterFirst, 30)
    val (_, afterThird)  = conduct(afterSecond, 30)

    val burden = afterThird
      .responses
      .groupMapReduce(_.participant)(_ => 1)(_ + _)
    assert(
      burden.values.max <= 6,
      s"one guest was asked ${ burden.values.max }",
    )
    assert(
      afterThird.responses.sizeIs <= 90,
      s"${ afterThird.responses.size } questions in all",
    )

  test("the leading slot falls in the month that most guests favour"):
    val (_, afterFirst)  = conduct(base, 30)
    val (_, afterSecond) = conduct(afterFirst, 30)
    val (_, afterThird)  = conduct(afterSecond, 30)
    val outcome          = Analysis.of(afterThird, 30)
    assertEquals(
      outcome.verdict.recommended.get.window.start.month,
      6,
    )
