package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.{Id, Slot}
import io.circe.Codec

/**
  * The advice arising from a poll: which slot to choose, how sure that advice
  * is, and what remains to be gained by asking anything further.
  *
  * The last of these is the one that decides when to stop. [[information]] is
  * what could still be learned, in weighted heads, and it bounds what any
  * further question could be worth; once it falls below what another round of
  * asking costs, the right move is to stop asking and book.
  *
  * @param forecasts
  *   A forecast for every slot, best first.
  *
  * @param recommended
  *   The slot to choose if a choice must be made now, or [[None]] if the poll
  *   has no slots.
  *
  * @param confidence
  *   For each slot, the probability that it is in fact the best choice. Where
  *   several slots tie in a sampled turnout, the credit is shared between them.
  *
  * @param regret
  *   For each slot, the expected shortfall in weighted attendance from choosing
  *   it rather than whichever slot turns out to be best, less the [[noise]]
  *   that no answer could remove.
  *
  * @param information
  *   What remains to be learned, in weighted heads: the expected gain from
  *   having everybody's answers rather than choosing now. Equal to the regret
  *   of the recommended slot.
  *
  * @param noise
  *   The part of the spread between slots that no amount of asking can remove,
  *   in weighted heads, being the chance that a guest who says they can attend
  *   does not in the end turn up.
  *
  * @param settled
  *   Whether [[information]] has fallen below the tolerance, meaning that no
  *   further round of questions can be worth its cost.
  */
final case class Verdict
  (
    forecasts: List[Forecast],
    recommended: Option[Slot],
    confidence: Map[Id[Slot], Double],
    regret: Map[Id[Slot], Double],
    information: Double,
    noise: Double,
    settled: Boolean,
  )
  derives Codec.AsObject:

  /** The forecast of the recommended slot. */
  def best: Option[Forecast] =
    recommended.flatMap(slot => forecasts.find(_.slot.id == slot.id))

  /**
    * The slots that remain plausible choices, being those with at least the
    * given chance of turning out to be best. Restricting attention to these is
    * what keeps later rounds of questioning cheap: there is no point asking
    * about a slot that has already lost.
    *
    * @param threshold
    *   The least confidence a slot must have to remain in contention.
    *
    * @return
    *   The contending slots, best first.
    */
  def contenders(threshold: Double = 0.02): List[Slot] = forecasts
    .map(_.slot)
    .filter(slot => confidence.getOrElse(slot.id, 0.0) >= threshold)

object Verdict:

  /**
    * The default tolerance, in weighted heads, below which further questioning
    * is judged not to be worth the asking.
    */
  val Tolerance: Double = 0.5

  /**
    * Derives advice from an ensemble of sampled worlds.
    *
    * Two ensembles are wanted, not one, and the reason is the whole difficulty
    * of knowing when to stop. Sampled turnouts vary for two quite different
    * reasons: because we do not know who can attend, which asking would fix,
    * and because a guest who can attend may still not come, which it would not.
    * Measuring the spread between slots picks up both at once, and the second
    * part does not shrink as answers arrive. Left uncorrected it forms a floor
    * that grows with the number of guests, of slots and of weights, so on a
    * poll of any realistic size the advice would be "ask another round" for
    * ever.
    *
    * The floor is therefore measured rather than assumed, by drawing a second
    * ensemble from the same belief once fully answered and subtracting what
    * remains. What is left is only what asking could still recover.
    *
    * @param ensemble
    *   The ensemble to draw conclusions from.
    *
    * @param saturated
    *   An ensemble drawn from [[Belief.saturated]], supplying the irreducible
    *   floor. Passing `ensemble` itself forgoes the correction.
    *
    * @param tolerance
    *   The value of information, in weighted heads, below which the choice is
    *   treated as settled.
    *
    * @return
    *   The advice arising from the ensemble.
    */
  def of
    (
      ensemble: Ensemble,
      saturated: Ensemble,
      tolerance: Double = Tolerance,
    )
    : Verdict =

    val slots     = ensemble.belief.slots
    val forecasts = Forecast.all(ensemble.belief, ensemble.objective)

    // The recommendation is the slot of greatest expected value, taken from the
    // exact forecast rather than from the average of the sampled worlds. The two
    // agree to within sampling error, but only the exact one is stable: where
    // several slots are closely matched, as they are before anybody has
    // answered, letting sampling noise choose between them would both give an
    // arbitrary answer and disagree with the order the slots are listed in.
    val best = forecasts
      .headOption
      .map(forecast => slots.indexWhere(_.id == forecast.slot.id))
      .filter(_ >= 0)

    val floor     = spread(saturated).map(_._2).minOption.getOrElse(0.0)
    val shortfall = spread(ensemble)

    val regret = slots
      .indices
      .map(slot => slots(slot).id -> math.max(0.0, shortfall(slot)._2 - floor))
      .toMap

    val information = best.map(slot => regret(slots(slot).id)).getOrElse(0.0)

    Verdict(
      forecasts = forecasts,
      recommended = best.map(slots),
      confidence =
        slots.indices.map(slot => slots(slot).id -> shortfall(slot)._1).toMap,
      regret = regret,
      information = information,
      noise = floor,
      settled = information < tolerance,
    )

  /**
    * For each slot, how often it comes out best across the ensemble and how far
    * short of the winner it falls on average.
    *
    * Where several slots tie for best in a world, the credit is shared between
    * them rather than awarded to whichever comes first. Ties are not a rarity
    * to be waved through: with cost ignored a slot's worth in a world is a sum
    * of integer weights, so before many answers are in, a large share of worlds
    * ends level, and giving every one of them to the lowest-numbered slot would
    * overstate it severalfold however many worlds were sampled.
    *
    * @param ensemble
    *   The ensemble to summarise.
    *
    * @return
    *   For each slot, its share of the wins and its mean shortfall.
    */
  private def spread(ensemble: Ensemble): IndexedSeq[(Double, Double)] =
    val slots = ensemble.belief.slots.indices

    val tallies = ensemble
      .worlds
      .map: world =>
        val scores  = slots.map(ensemble.score(world, _))
        val highest = scores.maxOption.getOrElse(0.0)
        val level   = scores.count(_ == highest)
        (
          slots.map(slot =>
            if scores(slot) == highest then 1.0 / level else 0.0,
          ),
          scores.map(highest - _),
        )

    slots.map: slot =>
      (
        tallies.map(_._1(slot)).sum / ensemble.size,
        tallies.map(_._2(slot)).sum / ensemble.size,
      )
