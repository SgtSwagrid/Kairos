package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.{Id, Slot}
import io.circe.Codec

/**
  * The advice arising from a poll: which slot to choose, how sure that advice
  * is, and what remains to be gained by asking anything further.
  *
  * The last of these is the one that decides when to stop. [[information]] is
  * the value of perfect information: the difference between what could be
  * achieved knowing everybody's availability exactly and what is achieved by
  * committing now. It is an upper bound on what any further question could
  * possibly be worth, so once it falls below what another round of asking
  * costs, the right move is to stop asking and book.
  *
  * @param forecasts
  *   A forecast for every slot, best first.
  *
  * @param recommended
  *   The slot to choose if a choice must be made now, or [[None]] if the poll
  *   has no slots.
  *
  * @param confidence
  *   For each slot, the probability that it is in fact the best choice.
  *
  * @param regret
  *   For each slot, the expected shortfall in weighted attendance from choosing
  *   it rather than whichever slot turns out to be best.
  *
  * @param information
  *   The expected gain, in weighted heads, from knowing everybody's
  *   availability perfectly rather than choosing now. Equal to the regret of
  *   the recommended slot.
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
  val Tolerance = 0.5

  /**
    * Derives advice from an ensemble of sampled worlds.
    *
    * @param ensemble
    *   The ensemble to draw conclusions from.
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
      tolerance: Double = Tolerance,
    )
    : Verdict =

    val slots     = ensemble.belief.slots
    val forecasts = Forecast.all(ensemble.belief, ensemble.objective)

    // The recommendation is the option of greatest expected value, taken from
    // the exact forecast rather than from the average of the sampled worlds.
    // The two agree to within sampling error, but only the exact one is stable:
    // where several options are closely matched, as they are before anybody has
    // answered, letting sampling noise pick between them would both give an
    // arbitrary answer and disagree with the order the options are listed in.
    val best = forecasts
      .headOption
      .map(_.slot.id)
      .flatMap(leader =>
        slots.indexWhere(_.id == leader) match
          case -1    => None
          case found => Some(found),
      )

    // In each world, which slot won and by how much it beat each of the others.
    val outcomes =
      if slots.isEmpty then IndexedSeq.empty
      else
        ensemble
          .worlds
          .map: world =>
            val scores  = slots.indices.map(ensemble.score(world, _))
            val highest = scores.max
            (scores.indexOf(highest), scores.map(highest - _))

    val wins      = outcomes.groupMapReduce(_._1)(_ => 1)(_ + _)
    val shortfall = slots
      .indices
      .map(slot => outcomes.map(_._2(slot)).sum / ensemble.size)

    val regret = slots
      .indices
      .map(slot => slots(slot).id -> shortfall(slot))
      .toMap

    val information = best.map(shortfall).getOrElse(0.0)

    Verdict(
      forecasts = forecasts,
      recommended = best.map(slots),
      confidence = slots
        .indices
        .map(slot =>
          slots(slot).id -> wins.getOrElse(slot, 0).toDouble / ensemble.size,
        )
        .toMap,
      regret = regret,
      information = information,
      settled = information < tolerance,
    )
