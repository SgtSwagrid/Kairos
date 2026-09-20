package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * What the organiser is trying to maximise, together with the modelling
  * assumptions used to predict it. Every quantity the solver reports is
  * expressed in the units of [[Objective]], namely weighted heads attending.
  *
  * @param costWeight
  *   How many weighted heads one unit of cost is worth giving up. Leave at `0`
  *   to ignore cost and choose purely on attendance.
  *
  * @param overflowWeight
  *   The penalty per head by which attendance is expected to exceed a slot's
  *   capacity. This should exceed `1`, because turning away a guest who has
  *   already accepted costs more than merely failing to invite them.
  *
  * @param prior
  *   The probability that an arbitrary participant can attend an arbitrary
  *   slot, before anything has been asked. This is the belief that screening
  *   questions displace, so a value near `0.5` is the honest choice unless
  *   there is reason to expect otherwise.
  *
  * @param dilution
  *   How much a positive answer to a pooled window question is discounted when
  *   applied to one slot inside it, as an exponent on the ratio of the slot's
  *   length to the window's. At `0` a positive pooled answer transfers in full,
  *   at `1` it is discounted in proportion to how much of the window the slot
  *   covers, and the default of `0.5` sits between the two.
  *
  * @param discretion
  *   How much dearer it is to ask someone about one named date than about a
  *   whole period, measured in questions. Naming a date is not a neutral act:
  *   it invites the answer to be read as a commitment, sets people checking
  *   flights and holding the weekend, and costs goodwill if the date is then
  *   dropped. Asking which months could work costs nothing but a moment's
  *   thought. At `1` the two are treated as equally cheap and named dates will
  *   almost always be asked about, since they are more informative; above `1`
  *   broad questions are preferred until a date is worth the asking. This is
  *   the one setting that governs how the tool behaves early on, so it is worth
  *   choosing deliberately.
  *
  * @param consistency
  *   The probability that a participant's ability to attend is governed by one
  *   underlying disposition common to every slot, rather than by independent
  *   chance at each. This does not alter any single prediction, but it governs
  *   how strongly predictions move together: at `0` a participant's misfortune
  *   at one slot says nothing about another, whereas at `1` the participant is
  *   simply available or not.
  */
final case class Objective
  (
    costWeight: Double = 0.0,
    overflowWeight: Double = 3.0,
    prior: Double = 0.5,
    dilution: Double = 0.5,
    discretion: Double = 3.0,
    consistency: Double = 0.5,
  )
  derives Codec.AsObject

object Objective:

  /** The default objective, which ignores cost and penalises overflow. */
  val default: Objective = Objective()
