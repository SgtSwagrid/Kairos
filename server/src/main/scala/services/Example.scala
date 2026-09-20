package com.alecdorrington.server
package services

import com.alecdorrington.common.api.{Draft, ParticipantDraft, VenueDraft}
import com.alecdorrington.common.model.{Day, Objective, Window}

/**
  * A worked example, so that the tool can be tried without first setting up a
  * poll of one's own. Three venues of differing size, cost and availability,
  * and a guest list with a few people who matter more than the rest.
  */
object Example:

  /** A window between two ISO dates. */
  private def between(from: String, to: String): Window =
    Window(Day.parse(from).get, Day.parse(to).get)

  /** The guests, four of whom the event cannot go ahead without. */
  private val guests: List[ParticipantDraft] =
    List("Ada", "Ben", "Cleo", "Dara").map(
      ParticipantDraft(_, weight = 10.0),
    ) ++ List(
      "Eli",
      "Fay",
      "Gus",
      "Hana",
      "Ivo",
      "Jun",
      "Kit",
      "Lena",
      "Milo",
      "Nia",
      "Omar",
      "Pia",
      "Quinn",
      "Rafa",
      "Sena",
      "Tariq",
      "Uma",
      "Vik",
      "Wren",
      "Xan",
      "Yara",
      "Zoë",
      "Arun",
      "Bea",
      "Caro",
      "Dev",
      "Esme",
      "Finn",
    ).map(ParticipantDraft(_))

  /** The example poll, awaiting its first round of questions. */
  val draft: Draft = Draft(
    title = "Mountain reunion",
    venues = List(
      VenueDraft(
        name = "Scheidegg Hut",
        openings = List(between("2027-06-04", "2027-07-18")),
        capacity = 28,
        cost = 5200,
      ),
      VenueDraft(
        name = "Lauterbrunnen Lodge",
        openings = List(
          between("2027-05-14", "2027-06-27"),
          between("2027-08-06", "2027-08-29"),
        ),
        capacity = 40,
        cost = 3800,
      ),
      VenueDraft(
        name = "Grindelwald Farmhouse",
        openings = List(between("2027-07-02", "2027-09-12")),
        capacity = 64,
        cost = 7400,
      ),
    ),
    participants = guests,
    length = 3,
    stride = 7,
    // Travelling far for three days is a real imposition, so most people cannot
    // make most weekends; and naming a date invites it to be held, so broad
    // questions come first.
    objective = Objective(prior = 0.25, discretion = 3.0),
  )
