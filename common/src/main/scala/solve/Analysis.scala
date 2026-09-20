package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.Poll
import io.circe.Codec

/**
  * Everything the solver has to say about a poll: what to choose, how sure it
  * is, and what to ask next.
  *
  * This is the single entry point to the solver. The pipeline is always the
  * same: interpret the answers as a [[Belief]], sample an [[Ensemble]] of
  * possible worlds from it, read the advice off the ensemble as a [[Verdict]],
  * and ask what question would most improve that advice as a [[Round]].
  *
  * @param verdict
  *   The advice arising from the answers received so far.
  *
  * @param round
  *   The questions worth asking next.
  *
  * @param answered
  *   The number of participants who have answered at least one question.
  *
  * @param worlds
  *   The number of possible worlds the advice was drawn from.
  */
final case class Analysis
  (
    verdict: Verdict,
    round: Round,
    answered: Int,
    worlds: Int,
  )
  derives Codec.AsObject

object Analysis:

  /**
    * Analyses a poll from end to end.
    *
    * @param poll
    *   The poll to analyse.
    *
    * @param budget
    *   The greatest number of questions the next round may contain.
    *
    * @param perParticipant
    *   The greatest number of questions to put to any one participant.
    *
    * @param tolerance
    *   The value of information, in weighted heads, below which the choice is
    *   treated as settled and no further round is worth sending.
    *
    * @return
    *   The solver's advice on the poll.
    */
  def of
    (
      poll: Poll,
      budget: Int = 40,
      perParticipant: Int = 3,
      tolerance: Double = Verdict.Tolerance,
    )
    : Analysis =

    val belief   = Belief.from(poll)
    val ensemble = Ensemble.draw(belief, poll.objective)
    val verdict  = Verdict.of(ensemble, tolerance)

    val round =
      if poll.slots.isEmpty || poll.participants.isEmpty then
        Round(List.empty, 0.0, verdict.information)
      else
        Elicitation.next(
          poll,
          ensemble,
          verdict,
          budget,
          perParticipant,
        )

    Analysis(
      verdict = verdict,
      round = round,
      answered = poll.participants.size - poll.silent.size,
      worlds = ensemble.size,
    )
