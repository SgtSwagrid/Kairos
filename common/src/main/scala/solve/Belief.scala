package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*

/**
  * What is currently believed about who can attend what: for every participant
  * and every slot, the probability that the participant attends if that slot is
  * chosen.
  *
  * This is the only place where answers are turned into numbers, and it is
  * deliberately the simplest component of the solver. Everything downstream —
  * ranking, risk, and the choice of what to ask next — reads from here, so
  * refining how answers are interpreted improves every other result at once.
  *
  * @param participants
  *   The participants, in the row order of [[probability]].
  *
  * @param slots
  *   The slots, in the column order of [[probability]].
  *
  * @param probability
  *   The attendance probability of each participant at each slot, indexed first
  *   by participant and then by slot.
  */
final case class Belief
  (
    participants: Vector[Participant],
    slots: Vector[Slot],
    probability: Vector[Vector[Double]],
  ):

  /** The probability that the given participant attends the given slot. */
  def apply(participant: Int, slot: Int): Double =
    probability(participant)(slot)

  /** The number of participants. */
  def size: Int = participants.size

  /** The number of candidate slots. */
  def width: Int = slots.size

  /**
    * How uncertain this belief is about the given participant and slot, at its
    * greatest where the probability is one half and zero where the answer is
    * settled. Used to avoid spending questions on people whose position is
    * already known.
    */
  def doubt(participant: Int, slot: Int): Double =
    val p = apply(participant, slot)
    4 * p * (1 - p)

object Belief:

  /**
    * Derives the current belief from a poll's answers.
    *
    * Direct answers about a slot are taken at face value. Where there is none,
    * the narrowest pooled answer enclosing the slot is used instead, discounted
    * according to [[Objective.dilution]]. Where there is neither, the prior
    * stands.
    *
    * @param poll
    *   The poll whose answers should be interpreted.
    *
    * @return
    *   A belief over every participant and slot of the poll.
    */
  def from(poll: Poll): Belief =
    val slots = poll.slots.toVector
    Belief(
      participants = poll.participants.toVector,
      slots = slots,
      probability = poll
        .participants
        .toVector
        .map: participant =>
          val answers = poll.responsesBy(participant.id)
          slots.map(slot => infer(answers, slot, poll.objective)),
    )

  /**
    * Infers the probability that a participant attends one slot, given every
    * answer they have provided.
    *
    * @param answers
    *   Every answer the participant has given, in any order.
    *
    * @param slot
    *   The slot whose attendance probability is wanted.
    *
    * @param objective
    *   The modelling assumptions to apply.
    *
    * @return
    *   The probability that the participant attends `slot`.
    */
  private def infer
    (
      answers: Seq[Response],
      slot: Slot,
      objective: Objective,
    )
    : Double =
    val bearing = answers.filter(_.question.bearsOn(slot))
    val direct  = bearing.collectFirst:
      case Response(_, Question.AboutSlot(_), availability) => availability
    val pooled = bearing
      .collect:
        case Response(
            _,
            Question.AboutWindow(window),
            availability,
          ) => window -> availability
      .minByOption(_._1.length)

    direct.map(_.probability) orElse pooled.map((window, availability) =>
      transfer(
        availability,
        slot.window,
        window,
        objective,
      ),
    ) getOrElse objective.prior

  /**
    * Transfers a pooled answer about a window onto one slot inside it.
    *
    * The transfer is asymmetric, and deliberately so. A negative answer applies
    * in full, because someone who cannot travel in June cannot travel on any
    * weekend in June; this is what makes wide screening questions so efficient.
    * A positive answer applies only in part, because being free at some point
    * in June is weaker evidence about one particular weekend, and the wider the
    * window the weaker it gets.
    *
    * @param availability
    *   The grade given in answer to the pooled question.
    *
    * @param slot
    *   The window occupied by the slot in question.
    *
    * @param window
    *   The wider window that was asked about, enclosing `slot`.
    *
    * @param objective
    *   The modelling assumptions to apply.
    *
    * @return
    *   The probability that the participant attends the slot.
    */
  private def transfer
    (
      availability: Availability,
      slot: Window,
      window: Window,
      objective: Objective,
    )
    : Double =
    val stated = availability.probability
    if stated <= objective.prior then stated
    else
      val breadth     = slot.length.toDouble / window.length
      val specificity = math.pow(
        math.min(1.0, breadth),
        objective.dilution,
      )
      objective.prior + (stated - objective.prior) * specificity
