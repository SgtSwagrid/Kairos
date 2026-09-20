package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.{Objective, Question}
import scala.util.Random

/**
  * A collection of possible worlds sampled from a belief, each recording who
  * attended each slot and what each slot was consequently worth.
  *
  * Sampling rather than solving is what makes the harder questions answerable.
  * The chance that a slot is the right choice, the loss from choosing wrongly,
  * and the worth of any question that might be asked next are all awkward to
  * derive in closed form, but each is a simple statistic of an ensemble.
  *
  * Within a world, a participant's fortunes at different slots are correlated,
  * governed by [[Objective.consistency]]: with some probability the participant
  * has a single underlying disposition that decides every slot alike, and
  * otherwise each slot is decided independently. Either way the marginal
  * probability of attending any given slot is exactly what the belief says, so
  * this adds dependence without distorting any individual prediction.
  *
  * @note
  *   This class stores its worlds in flat arrays and fills them with loops.
  *   That is a deliberate departure from the style of the rest of the codebase,
  *   confined to this file, because an ensemble holds millions of entries and
  *   the immutable equivalent is too slow to sit behind an interactive page.
  *   The arrays are never mutated after construction.
  */
final class Ensemble private (
  val belief: Belief,
  val objective: Objective,
  val size: Int,
  private val attending: Array[Boolean],
  private val scores: Array[Double],
):

  /** The number of candidate slots in each world. */
  def width: Int = belief.width

  /** Whether the given participant attends the given slot in the given world. */
  def attends(world: Int, participant: Int, slot: Int): Boolean =
    attending((world * belief.size + participant) * width + slot)

  /** What the given slot is worth in the given world. */
  def score(world: Int, slot: Int): Double = scores(world * width + slot)

  /**
    * The answer the given participant would give to the given question in the
    * given world, as a truth rather than a grade.
    *
    * A pooled question is answered by whether any enclosed slot would work,
    * which is precisely the pooled measurement of group testing: one answer
    * summarises many slots, and a negative answer rules out all of them.
    *
    * @param world
    *   The world in which to answer.
    *
    * @param participant
    *   The index of the participant answering.
    *
    * @param question
    *   The question being asked.
    *
    * @return
    *   `true` if the participant could attend what was asked about.
    */
  def answer
    (
      world: Int,
      participant: Int,
      question: Question,
    )
    : Boolean = any(world, participant, bearing(question))

  /**
    * Whether the given participant attends any of the given slots in the given
    * world. Callers in a loop should resolve [[bearing]] once and pass the
    * result here, rather than repeatedly re-deriving which slots a question
    * covers.
    *
    * @param world
    *   The world in which to answer.
    *
    * @param participant
    *   The index of the participant answering.
    *
    * @param slots
    *   The indices of the slots of interest.
    *
    * @return
    *   `true` if the participant attends at least one of `slots`.
    */
  def any
    (
      world: Int,
      participant: Int,
      slots: Seq[Int],
    )
    : Boolean = slots.exists(attends(world, participant, _))

  /** The indices of the slots that the given question bears upon. */
  def bearing(question: Question): Seq[Int] = belief
    .slots
    .indices
    .filter(slot => question.bearsOn(belief.slots(slot)))

  /** The mean score of the given slot across every world. */
  def meanScore(slot: Int): Double = (0 until size).map(score(_, slot)).sum /
    size

  /** Every world index, for folding over the ensemble. */
  def worlds: Range = 0 until size

object Ensemble:

  /** The greatest number of world entries to hold, bounding memory use. */
  private val Budget = 6000000

  /**
    * Samples an ensemble of worlds from a belief.
    *
    * @param belief
    *   The belief to sample from.
    *
    * @param objective
    *   The objective by which each world's slots are scored.
    *
    * @param seed
    *   The seed for the random number generator, fixed by default so that the
    *   same poll always yields the same advice.
    *
    * @return
    *   An ensemble of worlds drawn from `belief`.
    */
  def draw
    (
      belief: Belief,
      objective: Objective,
      seed: Long = 20260920L,
    )
    : Ensemble =

    val people = belief.size
    val slots  = belief.width
    val worlds = capacity(people, slots)
    val random = Random(seed)

    // Flatten the inputs, so that the sampling loop touches only arrays.
    val chance = Array.tabulate(people * slots): entry =>
      belief(entry / slots, entry % slots)
    val weight     = belief.participants.map(_.weight).toArray
    val capacities = belief.slots.map(_.capacity).toArray
    val overhead   = belief
      .slots
      .map(slot => objective.costWeight * slot.cost)
      .toArray

    val attending = new Array[Boolean](worlds * people * slots)
    val scores    = new Array[Double](worlds * slots)

    var world = 0
    while world < worlds do

      val heads = new Array[Int](slots)
      val value = new Array[Double](slots)

      var person = 0
      while person < people do

        // Either one disposition governs every slot, or each is decided alone.
        val disposition = random.nextDouble()
        val governed    = random.nextDouble() < objective.consistency

        var slot = 0
        while slot < slots do
          val draw = if governed then disposition else random.nextDouble()
          if draw < chance(person * slots + slot) then
            attending((world * people + person) * slots + slot) = true
            heads(slot) += 1
            value(slot) += weight(person)
          slot += 1

        person += 1

      var slot = 0
      while slot < slots do
        val excess = math.max(0, heads(slot) - capacities(slot))
        scores(world * slots + slot) = value(slot) - overhead(slot) -
          objective.overflowWeight * excess
        slot += 1

      world += 1

    new Ensemble(
      belief,
      objective,
      worlds,
      attending,
      scores,
    )

  /**
    * The number of worlds to sample, reduced for larger polls so that the
    * ensemble stays within its memory budget.
    *
    * @param people
    *   The number of participants.
    *
    * @param slots
    *   The number of candidate slots.
    *
    * @return
    *   A number of worlds between `500` and `3000`.
    */
  private def capacity(people: Int, slots: Int): Int =
    val entries = math.max(1, people * slots)
    math.max(500, math.min(3000, Budget / entries))
