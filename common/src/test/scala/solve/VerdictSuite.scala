package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*
import com.alecdorrington.common.solve.Fixture.*
import munit.FunSuite

class VerdictSuite extends FunSuite:

  private val good = slot("Good", "2027-06-11")
  private val bad  = slot("Bad", "2027-06-18")

  private val guests = (1 to 12)
    .map(index => participant(s"guest$index"))
    .toList

  private def verdict(responses: List[Response]): Verdict =
    val subject = poll(List(good, bad), guests).copy(responses = responses)
    Verdict.of(Ensemble.draw(
      Belief.from(subject),
      subject.objective,
    ))

  test("confidence is a distribution over the slots"):
    val advice = verdict(List.empty)
    assertEqualsDouble(
      advice.confidence.values.sum,
      1.0,
      1.0e-9,
    )
    assertEquals(advice.confidence.size, 2)

  test("the value of information is the regret of the recommendation"):
    val advice = verdict(List.empty)
    val chosen = advice.recommended.get
    assertEqualsDouble(
      advice.information,
      advice.regret(chosen.id),
      1.0e-9,
    )

  test("regret is never negative and vanishes for some slot"):
    val advice = verdict(List.empty)
    assert(advice.regret.values.forall(_ >= -1.0e-9))
    assert(advice.regret.values.min >= 0.0)

  test("knowing nothing leaves much to learn and settles nothing"):
    val advice = verdict(List.empty)
    assert(
      advice.information > Verdict.Tolerance,
      advice.information,
    )
    assert(!advice.settled)
    assert(
      advice.confidence.values.max < 0.9,
      "no slot should look certain",
    )

  test("a unanimous answer settles the choice"):
    val answers = guests.flatMap: guest =>
      List(
        says(guest.name, good, Availability.Yes),
        says(guest.name, bad, Availability.No),
      )
    val advice = verdict(answers)
    assertEquals(
      advice.recommended.map(_.venue),
      Some("Good"),
    )
    assert(
      advice.confidence(good.id) > 0.98,
      advice.confidence(good.id),
    )
    assert(
      advice.information < Verdict.Tolerance,
      advice.information,
    )
    assert(advice.settled)

  test("forecasts are ordered best first and agree with the recommendation"):
    val answers = guests.map(guest => says(guest.name, good, Availability.Yes))
    val advice  = verdict(answers)
    val scores  = advice.forecasts.map(_.score)
    assertEquals(scores, scores.sorted.reverse)
    assertEquals(
      advice.forecasts.head.slot.id,
      advice.recommended.get.id,
    )
    assertEquals(
      advice.best.map(_.slot.venue),
      Some("Good"),
    )

  test("popularity does not redeem a venue that cannot hold everyone"):
    val cramped = slot("Cramped", "2027-06-11", capacity = 3)
    val roomy   = slot("Roomy", "2027-06-18", capacity = 50)
    val answers = guests.flatMap: guest =>
      List(
        says(guest.name, cramped, Availability.Yes),
        says(
          guest.name,
          roomy,
          Availability.Probably,
        ),
      )
    val subject = poll(List(cramped, roomy), guests).copy(responses = answers)
    val advice  = Verdict.of(Ensemble.draw(
      Belief.from(subject),
      subject.objective,
    ))
    assertEquals(
      advice.recommended.map(_.venue),
      Some("Roomy"),
    )

  test("contenders exclude slots that have already lost"):
    val answers = guests.flatMap: guest =>
      List(
        says(guest.name, good, Availability.Yes),
        says(guest.name, bad, Availability.No),
      )
    assertEquals(
      verdict(answers).contenders().map(_.venue),
      List("Good"),
    )
    assertEquals(
      verdict(List.empty).contenders().size,
      2,
    )

  test("an empty poll is handled without complaint"):
    val empty  = poll(List.empty, List.empty)
    val advice = Verdict.of(Ensemble.draw(Belief.from(empty), empty.objective))
    assertEquals(advice.recommended, None)
    assertEquals(advice.forecasts, List.empty)
    assertEqualsDouble(advice.information, 0.0, 1.0e-9)
