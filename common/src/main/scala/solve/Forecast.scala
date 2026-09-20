package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.{Objective, Slot}
import io.circe.Codec

/**
  * What is expected to happen if one particular slot is chosen.
  *
  * The attendance distribution is exact rather than simulated. Because
  * participants are modelled as attending independently of one another, the
  * number who turn up is Poisson-binomial, and its distribution can be computed
  * in full by convolution. This matters for capacity: the probability of
  * overrunning a venue is a tail property, and reading a tail off a sample is
  * far less reliable than computing it.
  *
  * @param slot
  *   The slot being forecast.
  *
  * @param attendance
  *   The expected number of participants who attend.
  *
  * @param weighted
  *   The expected attendance, counting each participant by their weight. This
  *   is the quantity the organiser is trying to maximise.
  *
  * @param overflow
  *   The expected number of participants by which attendance exceeds capacity.
  *
  * @param risk
  *   The probability that attendance exceeds capacity at all.
  *
  * @param score
  *   The expected value of choosing this slot, in weighted heads, after
  *   deducting the penalties for cost and for overflow.
  *
  * @param distribution
  *   The probability of each possible number of attendees, indexed by that
  *   number from zero.
  */
final case class Forecast
  (
    slot: Slot,
    attendance: Double,
    weighted: Double,
    overflow: Double,
    risk: Double,
    score: Double,
    distribution: Vector[Double],
  )
  derives Codec.AsObject:

  /** The proportion of this slot's capacity that is expected to be used. */
  def utilisation: Double =
    if slot.capacity <= 0 then 0.0 else attendance / slot.capacity

object Forecast:

  /**
    * Forecasts every slot of a belief, in descending order of score.
    *
    * @param belief
    *   The belief to forecast from.
    *
    * @param objective
    *   The objective against which slots are scored.
    *
    * @return
    *   One forecast per slot, best first.
    */
  def all(belief: Belief, objective: Objective): List[Forecast] = belief
    .slots
    .indices
    .map(slot => of(belief, slot, objective))
    .sortBy(-_.score)
    .toList

  /**
    * Forecasts one slot of a belief.
    *
    * @param belief
    *   The belief to forecast from.
    *
    * @param slot
    *   The index of the slot to forecast.
    *
    * @param objective
    *   The objective against which the slot is scored.
    *
    * @return
    *   A forecast for that slot.
    */
  def of
    (
      belief: Belief,
      slot: Int,
      objective: Objective,
    )
    : Forecast =
    val candidate     = belief.slots(slot)
    val probabilities = belief.participants.indices.map(belief(_, slot))
    val distribution  = poissonBinomial(probabilities)
    val weighted      = belief
      .participants
      .indices
      .map(person => belief.participants(person).weight * belief(person, slot))
      .sum

    Forecast(
      slot = candidate,
      attendance = probabilities.sum,
      weighted = weighted,
      overflow = excess(distribution, candidate.capacity),
      risk = distribution.drop(candidate.capacity + 1).sum,
      score = weighted - objective.costWeight * candidate.cost -
        objective.overflowWeight * excess(distribution, candidate.capacity),
      distribution = distribution,
    )

  /**
    * The distribution of the number of successes among independent trials of
    * differing probability, computed by convolving one trial at a time.
    *
    * @param probabilities
    *   The success probability of each trial.
    *
    * @return
    *   The probability of each possible number of successes, indexed by that
    *   number from zero.
    */
  def poissonBinomial(probabilities: Seq[Double]): Vector[Double] =
    probabilities.foldLeft(Vector(1.0)): (distribution, probability) =>
      val unchanged = distribution :+ 0.0
      val advanced  = 0.0 +: distribution
      unchanged
        .lazyZip(advanced)
        .map((absent, present) =>
          absent * (1 - probability) + present * probability,
        )
        .toVector

  /**
    * The expected amount by which a count exceeds a threshold.
    *
    * @param distribution
    *   The probability of each possible count, indexed by that count.
    *
    * @param capacity
    *   The threshold beyond which the count is in excess.
    *
    * @return
    *   The expectation of the count less the threshold, floored at zero.
    */
  private def excess
    (
      distribution: Vector[Double],
      capacity: Int,
    )
    : Double = distribution
    .zipWithIndex
    .collect:
      case (probability, count) if count > capacity =>
        probability * (count - capacity)
    .sum
