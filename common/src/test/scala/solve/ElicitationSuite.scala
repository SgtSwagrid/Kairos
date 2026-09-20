package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*
import com.alecdorrington.common.solve.Fixture.*
import munit.FunSuite
import scala.concurrent.duration.{Duration, DurationInt}

class ElicitationSuite extends FunSuite:

  /**
    * Generous, because these run on Scala.js too, where the solver is several
    * times slower, and on a loaded build machine slower again.
    */
  override def munitTimeout: Duration = 5.minutes

  private val slots = Slot.enumerate(
    venue = "Hut",
    openings = List(window("2027-05-01", "2027-08-31")),
    capacity = 60,
    cost = 0,
    length = 3,
    stride = 7,
  )

  private val guests = (1 to 20)
    .map(index => participant(s"guest$index"))
    .toList

  private val base = poll(slots, guests)

  private def round
    (
      subject: Poll,
      budget: Int = 20,
      perParticipant: Int = 2,
    )
    : Round = Analysis.of(subject, budget, perParticipant).round

  /**
    * The proportion of a round's questions that are pooled rather than
    * specific.
    */
  private def pooling(subject: Poll): Double =
    val enquiries = round(subject, budget = 20, perParticipant = 2).enquiries
    assert(
      enquiries.nonEmpty,
      "something should be asked",
    )
    val pooled = enquiries.count:
      case Enquiry(_, Question.AboutWindow(_), _) => true
      case _                                      => false
    pooled.toDouble / enquiries.size

  test("broad questions come first while naming a date is costly"):
    // A named date is more informative but dearer to ask, so screening wins out
    // early, exactly as in group testing: one refusal for a whole month rules out
    // every weekend in it at a stroke, and nobody has been asked to hold a date.
    val careful = poll(
      slots,
      guests,
      Objective(prior = 0.2, discretion = 3.0),
    )
    assert(
      pooling(careful) >= 0.85,
      pooling(careful),
    )

  test("named dates come first once there is no cost to naming one"):
    val blunt = poll(
      slots,
      guests,
      Objective(prior = 0.2, discretion = 1.0),
    )
    assert(pooling(blunt) < 0.1, pooling(blunt))

  test("discretion governs how readily a date is named"):
    def asked(discretion: Double): Double = pooling(poll(
      slots,
      guests,
      Objective(prior = 0.3, discretion = discretion),
    ))
    assert(
      asked(6.0) >= asked(2.0),
      s"${ asked(6.0) } should be at least ${ asked(2.0) }",
    )
    assert(
      asked(2.0) >= asked(1.0),
      s"${ asked(2.0) } should be at least ${ asked(1.0) }",
    )

  test("once the broad questions are answered, dates are named"):
    // Every broad question, not merely every month: a slot lying across a month
    // boundary gets a window of its own, which is still a broad question.
    val broad = Question
      .candidates(slots)
      .collect:
        case Question.AboutWindow(covering) => covering
      .flatMap(covering =>
        guests.map(guest =>
          saysOf(
            guest.name,
            covering,
            Availability.Probably,
          ),
        ),
      )
    val followUp = round(base.copy(responses = broad), budget = 20)
    assert(
      followUp
        .enquiries
        .forall:
          case Enquiry(_, Question.AboutSlot(_), _) => true
          case _                                    => false,
      "with every broad question already answered, only dates should be left, " +
        s"but got ${ followUp
            .enquiries
            .collect { case Enquiry(who, Question.AboutWindow(w), _) =>
              s"${ who.value }:${ w.show }"
            }
            .take(3) }",
    )

  test("a round respects its budget and its per-participant limit"):
    val chosen = round(base, budget = 15, perParticipant = 2)
    assert(
      chosen.enquiries.sizeIs <= 15,
      chosen.enquiries.size,
    )
    chosen
      .byParticipant
      .foreach: (who, asked) =>
        assert(
          asked.sizeIs <= 2,
          s"$who was asked ${ asked.size } questions",
        )

  test("a round spreads its questions across many participants"):
    val chosen = round(base, budget = 20, perParticipant = 2)
    assert(
      chosen.recipients >= 8,
      s"only ${ chosen.recipients } participants would be contacted",
    )

  test("no question is put to the same participant twice in a round"):
    val chosen = round(base, budget = 30, perParticipant = 3).enquiries
    val pairs  = chosen.map(enquiry => enquiry.participant -> enquiry.question)
    assertEquals(pairs.distinct.size, pairs.size)

  test("a question already answered is never asked again"):
    val first   = round(base, budget = 10).enquiries
    val answers = first.map(enquiry =>
      Response(
        enquiry.participant,
        enquiry.question,
        Availability.Yes,
      ),
    )
    val second = round(base.record(answers), budget = 10).enquiries
    val asked  = first.map(e => e.participant -> e.question).toSet
    second.foreach: enquiry =>
      assert(
        !asked(enquiry.participant -> enquiry.question),
        enquiry.toString,
      )

  test("a round cannot resolve more than is left to resolve"):
    // Both sides are in bits. Comparing a round's worth against the verdict's
    // value of information would set bits against weighted heads, two things the
    // solver is at pains to keep apart.
    val analysis = Analysis.of(base, budget = 20, perParticipant = 2)
    assert(
      analysis.round.worth <= analysis.round.available + 1.0e-6,
      s"${ analysis.round.worth } exceeded ${ analysis.round.available }",
    )
    assert(
      analysis.round.worth > 0.0,
      "and it should be worth something",
    )
    assert(analysis.round.coverage <= 1.0 + 1.0e-6)

  test("every question chosen is worth something"):
    val chosen = round(base, budget = 20).enquiries
    assert(chosen.nonEmpty)
    assert(chosen.forall(_.worth > 0.0))

  test("a second question to the same guest may be worth more than the first"):
    // Not a defect but a property of the measure. Knowing whether someone could
    // travel in May says little on its own; knowing that and whether they could
    // travel in June says a good deal more than the sum of the two. Mutual
    // information is submodular across participants, whose answers are
    // independent, but not within one, so the worth of a round's questions does
    // not decrease along the way.
    val chosen = round(base, budget = 20, perParticipant = 2).enquiries
    val pairs  = chosen
      .groupBy(_.participant)
      .values
      .collect:
        case first :: second :: _ => (first.worth, second.worth)
    assert(
      pairs.nonEmpty,
      "some guest should have been asked twice",
    )
    assert(
      pairs.exists((first, second) => second > first),
      "complementary questions should show up as an increase",
    )

  test("nothing is asked once the answers are all in"):
    val answers =
      for
        guest     <- guests
        candidate <- slots
      yield says(guest.name, candidate, Availability.Yes)
    val chosen = round(base.copy(responses = answers))
    assertEquals(chosen.enquiries, List.empty)
    assertEqualsDouble(chosen.worth, 0.0, 1.0e-9)

  test("a participant who has answered about every slot is left alone"):
    val answers = slots.map(says("guest1", _, Availability.Probably))
    val chosen  = round(
      base.copy(responses = answers),
      budget = 40,
    )
    assert(
      !chosen.enquiries.exists(_.participant == Id[Participant]("guest1")),
      "there is nothing left to ask them that they have not already told us",
    )
    assert(
      chosen.enquiries.nonEmpty,
      "but everybody else is still worth asking",
    )

  test("a poll with no participants yields no questions"):
    assertEquals(
      round(poll(slots, List.empty)).enquiries,
      List.empty,
    )

  test("a poll with no slots yields no questions"):
    assertEquals(
      round(poll(List.empty, guests)).enquiries,
      List.empty,
    )

  test("essential guests are asked before ordinary ones"):
    val mixed  = guests.take(18) :+ participant("vip", weight = 12.0)
    val chosen = round(
      poll(slots, mixed),
      budget = 6,
      perParticipant = 1,
    )
    assert(
      chosen.enquiries.exists(_.participant == Id[Participant]("vip")),
      "a heavily weighted guest should be among the first asked",
    )

  test("every guest gets a candidate question, however many there are"):
    // The shortlist is taken a rank at a time across participants, so a flat cut
    // below their number would leave the same people unasked every round.
    val crowd  = (1 to 500).map(index => participant(s"guest$index")).toList
    val chosen = round(
      poll(slots, crowd),
      budget = 30,
      perParticipant = 1,
    )
    assert(
      chosen.recipients >= 20,
      s"only ${ chosen.recipients } contacted",
    )

  test("a free named date does not cut a round short"):
    // Ranking by worth per unit of cost, with the veto applied after, let a
    // question worth almost nothing win on a vanishing cost and then fail the
    // veto, ending the round with informative questions unasked.
    val blunt = poll(
      slots,
      guests,
      Objective(discretion = 0.0),
    )
    val chosen = round(blunt, budget = 20, perParticipant = 2)
    assert(
      chosen.enquiries.sizeIs >= 15,
      s"only ${ chosen.enquiries.size } asked",
    )
    assert(chosen.enquiries.forall(_.worth > 0.0))

  test("a nonsensical objective is bounded rather than obeyed"):
    val absurd = poll(
      slots,
      guests,
      Objective(
        prior = 4.0,
        dilution = -5.0,
        consistency = 9.0,
      ),
    )
    val analysis = Analysis.of(absurd, budget = 10, perParticipant = 1)
    analysis
      .verdict
      .forecasts
      .foreach: forecast =>
        assert(
          forecast.risk >= 0.0 && forecast.risk <= 1.0,
          s"risk of ${ forecast.risk }",
        )
        assert(
          !forecast.score.isNaN,
          "the score should be a number",
        )
        assertEqualsDouble(forecast.distribution.sum, 1.0, 1.0e-6)
