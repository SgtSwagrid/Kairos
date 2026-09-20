package com.alecdorrington.common
package solve

import com.alecdorrington.common.model.*
import io.circe.Codec

/**
  * One question worth putting to one participant, and what an answer is worth.
  *
  * @param participant
  *   The participant to ask.
  *
  * @param question
  *   The question to put to them.
  *
  * @param value
  *   How much this answer is expected to tell us about which option is best, in
  *   bits, given everything else being asked of the same participant in this
  *   round. One bit is the information in a perfectly balanced yes-or-no.
  */
final case class Enquiry
  (
    participant: Id[Participant],
    question: Question,
    value: Double,
  )
  derives Codec.AsObject

/**
  * A round of questions to send out, chosen together.
  *
  * @param enquiries
  *   The questions to ask, in the order they were chosen. Note that this is not
  *   descending order of worth: see [[Elicitation.next]] on complementary
  *   questions.
  *
  * @param value
  *   How much this round is expected to tell us about which option is best, in
  *   bits. This is the sum of what the individual answers are worth, capped at
  *   [[available]]. Summing is optimistic, because two participants' answers
  *   may in part tell us the same thing; the total is reported to convey the
  *   scale of a round, whereas it is the worth of each question that decides
  *   what is asked.
  *
  * @param available
  *   How uncertain it currently is which option is best, in bits, and so the
  *   most that any amount of asking could resolve. This measures uncertainty
  *   about the choice, not what choosing wrongly would cost; for the latter,
  *   and so for deciding whether to stop asking altogether, see
  *   [[Verdict.information]].
  */
final case class Round
  (
    enquiries: List[Enquiry],
    value: Double,
    available: Double,
  )
  derives Codec.AsObject:

  /** The questions to ask, grouped by the participant who should answer them. */
  def byParticipant: Map[Id[Participant], List[Enquiry]] =
    enquiries.groupBy(_.participant)

  /** The number of participants this round would contact. */
  def recipients: Int = enquiries.map(_.participant).distinct.size

  /**
    * The fraction of the remaining uncertainty about the choice that this round
    * would resolve. A round approaching `1` leaves little reason for another.
    */
  def coverage: Double =
    if available <= 0 then 1.0 else math.min(1.0, value / available)

object Elicitation:

  /** The greatest number of candidate questions to evaluate at each step. */
  private val Shortlist: Int = 400

  /**
    * The greatest number of slots to weigh against one another.
    *
    * [[uncertainty]] also packs a group and a winner into one key using this as
    * the radix, which is sound only because a winner is an index into the
    * contenders and so is always below it. The two uses are tied together; a
    * larger cap is free, a smaller one must stay above every winner.
    */
  private val MaxContenders: Int = 12

  /** The least worth, in bits, for which a question is worth anybody's time. */
  private val Worthwhile: Double = 1.0e-6

  /** The greatest number of worlds to weigh a question against. */
  private val Resolution: Int = 1000

  /** The fewest worlds a group must hold before it is worth subdividing. */
  private val Grain: Int = 25

  /**
    * Chooses the questions worth asking next.
    *
    * The method is Bayesian experimental design, evaluated on the ensemble. A
    * question's worth is how much it would tell us about which option is best:
    * partition the sampled worlds by the answer each implies, and measure how
    * much less uncertain the identity of the winning option becomes once the
    * answer is known. This is the mutual information between the answer and the
    * decision, which is what the literature on decision-region determination
    * maximises, and it is not the same thing as learning everybody's diary: a
    * question is worth asking only in so far as it bears on the choice.
    *
    * It is tempting instead to score a question by the expected improvement in
    * the choice itself, in guests rather than bits, and that was tried. It
    * fails in a way worth recording. One person's answer seldom overturns which
    * option leads, and where it does not, the improvement is not merely small
    * but exactly zero. Among thirty guests most questions then score nothing at
    * all, the search cannot tell them apart, and a round stops a few questions
    * in while a great deal remains unknown. Mutual information has no such
    * blind spot, being positive whenever an answer bears on the outcome, and so
    * ranks questions sensibly long before any one of them could swing the
    * decision.
    *
    * Questions are chosen as a set rather than one at a time, because a batch
    * is what actually gets sent out, and a batch must not ask the same thing
    * twice over. Each successive choice for a participant is judged against the
    * partition already induced by what that participant has been asked earlier
    * in the round, so following "could you travel in June?" with "could you
    * come on the second weekend of June?" earns only what it adds.
    *
    * Choosing the best set of questions outright is NP-hard, so this is the
    * usual greedy substitute. It is worth being straight about what that buys.
    * Greedy comes within a known factor of the best possible set when the
    * objective is submodular, and mutual information is submodular across
    * participants, whose answers are independent, but not within one: answers
    * can be complementary, so that "could you travel in May?" and "could you
    * travel in June?" are worth more together than the sum of their parts, and
    * the second may score higher than the first did. The worth of each question
    * reported here is therefore genuinely its marginal contribution, but the
    * sequence is not descending and the round carries no approximation
    * guarantee. In practice this is a good deal better than it sounds, since
    * the breadth of a round is governed by `perParticipant` rather than by the
    * objective's shape.
    *
    * Redundancy is accounted for within a participant but not between
    * participants, which is deliberate. Two guests answering the same question
    * are two independent draws on the thing we actually want to know, so there
    * is little for a joint treatment to discount; and conditioning jointly
    * across a whole round is not merely expensive but self-defeating, since
    * each answer conditioned upon halves the worlds left to judge the next one
    * against, and the ensemble is exhausted after about five questions however
    * large the batch.
    *
    * Three cheap restrictions keep the search affordable. Only options still in
    * contention are weighed, since resolving a choice between options that have
    * already lost is worthless. Only participants whose answers are still
    * uncertain are asked, since a settled answer reveals nothing. And questions
    * are weighed against a subsample of the ensemble, since ranking them needs
    * far less precision than estimating the risk of overrunning a venue.
    *
    * @param poll
    *   The poll being conducted, whose existing answers are never asked again.
    *
    * @param ensemble
    *   The ensemble of sampled worlds to evaluate against.
    *
    * @param verdict
    *   The current advice, used to identify which options remain in contention.
    *
    * @param budget
    *   The greatest number of questions to ask in total across the round.
    *
    * @param perParticipant
    *   The greatest number of questions to put to any one participant.
    *
    * @return
    *   The round of questions to send out.
    */
  def next
    (
      poll: Poll,
      ensemble: Ensemble,
      verdict: Verdict,
      budget: Int = 40,
      perParticipant: Int = 3,
    )
    : Round =

    val contending = contenders(ensemble, verdict)
    val sampled    = subsample(ensemble)
    val asked      = poll.responses.map(r => r.participant -> r.question).toSet
    val candidates = shortlist(
      poll,
      ensemble,
      sampled,
      contending,
      asked,
    )

    // Which option wins in each world. This is all the decision depends upon and
    // does not change as the round is assembled, so it is worked out once.
    val winners = victors(ensemble, sampled, contending)

    // Every world in one group, which is where each participant starts.
    val whole = Array.fill(sampled.length)(0)
    val plain = uncertainty(whole, winners)

    val start = (Map.empty[Int, Array[Int]], List.empty[Enquiry])

    val (_, chosen) = (1 to budget).foldLeft(start):
      case ((groups, chosen), _) =>
        val spent     = chosen.groupMapReduce(_.participant)(_ => 1)(_ + _)
        val taken     = chosen.map(e => e.participant -> e.question).toSet
        val baselines = groups.view.mapValues(uncertainty(_, winners)).toMap

        candidates
          .filter: (participant, question, _) =>
            val who = poll.participants(participant).id
            spent.getOrElse(who, 0) < perParticipant && !taken(who -> question)
          .map: (participant, question, answers) =>
            val refined = subdivide(
              groups.getOrElse(participant, whole),
              answers,
            )
            val gain = baselines.getOrElse(participant, plain) -
              uncertainty(refined, winners)
            (participant, question, refined, gain, gain / cost(question, poll))
          .filter(_._4 > Worthwhile)
          .maxByOption(_._5) match
            case None => (groups, chosen)
            case Some((participant, question, refined, gain, _)) =>
              val who = poll.participants(participant).id
              (
                groups.updated(participant, refined),
                Enquiry(who, question, gain) :: chosen,
              )

    val enquiries = chosen.reverse
    Round(
      enquiries = enquiries,
      value = math.min(enquiries.map(_.value).sum, plain),
      available = plain,
    )

  /**
    * What it costs to put a question to someone, in units of one broad
    * question.
    *
    * Questions are chosen by what they are worth per unit of this cost, rather
    * than by what they are worth outright. Without it the choice would always
    * fall on named dates, which are strictly more informative per question: a
    * named date settles that date exactly, whereas knowing that some weekend in
    * June would work tells us only a little about each weekend in June. That is
    * a true account of the information and a poor account of the cost, since
    * naming a date to a guest is the expensive part.
    *
    * @param question
    *   The question being considered.
    *
    * @param poll
    *   The poll being conducted, supplying the organiser's discretion.
    *
    * @return
    *   The cost of asking, never zero.
    */
  private def cost(question: Question, poll: Poll): Double = question match
    case Question.AboutWindow(_) => 1.0
    case Question.AboutSlot(_)   => poll.bounds.discretion

  /**
    * The worlds against which questions are weighed, thinned evenly to at most
    * [[Resolution]] of them.
    *
    * @param ensemble
    *   The ensemble to thin.
    *
    * @return
    *   The indices of the worlds to use.
    */
  private def subsample(ensemble: Ensemble): Array[Int] =
    val stride = math.max(1, ensemble.size / Resolution)
    ensemble.worlds.by(stride).toArray

  /**
    * The indices of the options still in contention, always at least two so
    * that there is a choice left to resolve.
    *
    * @param ensemble
    *   The ensemble whose options are being filtered.
    *
    * @param verdict
    *   The current advice, supplying each option's confidence.
    *
    * @return
    *   The indices of the contending options.
    */
  private def contenders(ensemble: Ensemble, verdict: Verdict): Vector[Int] =
    def confidence(slot: Int): Double = verdict
      .confidence
      .getOrElse(ensemble.belief.slots(slot).id, 0.0)
    // Ranked by expected value first, so that where several slots are equally
    // confident — which before anybody answers is nearly all of them — the ones
    // kept are the ones actually worth choosing between, rather than whichever
    // the venues happened to be listed in.
    val worth = verdict
      .forecasts
      .zipWithIndex
      .map((forecast, rank) => forecast.slot.id -> rank)
      .toMap
    val ranked = ensemble
      .belief
      .slots
      .indices
      .sortBy(slot =>
        (-confidence(slot), worth.getOrElse(ensemble.belief.slots(slot).id, 0)),
      )
    val plausible = ranked.filter(confidence(_) >= 0.01)
    (if plausible.sizeIs >= 2 then plausible else ranked.take(2))
      .take(MaxContenders)
      .toVector

  /**
    * The questions worth evaluating, each paired with the answer it would
    * receive in every world under consideration.
    *
    * Precomputing the answers is what makes the search affordable: the answer a
    * question would get in a given world never changes as the round is built
    * up, so it is derived once and reused at every step.
    *
    * @param poll
    *   The poll being conducted.
    *
    * @param ensemble
    *   The ensemble of sampled worlds.
    *
    * @param sampled
    *   The indices of the worlds under consideration.
    *
    * @param contending
    *   The indices of the options still in contention.
    *
    * @param asked
    *   The questions each participant has already answered.
    *
    * @return
    *   Triples of participant index, question, and the answer in each world.
    */
  private def shortlist
    (
      poll: Poll,
      ensemble: Ensemble,
      sampled: Array[Int],
      contending: Vector[Int],
      asked: Set[(Id[Participant], Question)],
    )
    : Vector[(Int, Question, Array[Boolean])] =

    val relevant = Question
      .candidates(poll.slots)
      .filter(question =>
        contending.exists(slot => question.bearsOn(ensemble.belief.slots(slot))),
      )
      .map(question => question -> ensemble.bearing(question))

    // A question is worth nothing if every option it covers has already been
    // answered about directly, since its answer is then merely implied by what
    // has been said. The ensemble cannot see this for itself: it would still
    // find some variation to resolve, but that residue is the chance of a guest
    // who can attend not turning up, which no amount of asking will settle.
    val settled = poll
      .responses
      .collect:
        case Response(who, Question.AboutSlot(slot), _) => who -> slot
      .toSet

    val pairs =
      for
        participant <- poll.participants.indices.toVector
        who = poll.participants(participant).id
        (question, bearing) <- relevant if !asked(who -> question)
        if !bearing.forall(slot =>
          settled(who -> ensemble.belief.slots(slot).id),
        )
        doubt = bearing.map(ensemble.belief.doubt(participant, _)).maxOption
        if doubt.exists(_ > 0.02)
      yield (
        participant,
        question,
        bearing,
        doubt.getOrElse(0.0) * poll.participants(participant).weight,
      )

    // Take the candidates in rounds across participants rather than simply the
    // most promising overall. Early on every participant is equally uncertain,
    // and a flat cut would then silently exclude everybody past the limit,
    // leaving whole swathes of the guest list unasked for no reason.
    val byParticipant = pairs
      .groupBy(_._1)
      .toVector
      .sortBy(_._1)
      .map(_._2.sortBy(-_._4))
    val depth = byParticipant.map(_.size).maxOption.getOrElse(0)

    // At least as many as there are participants, so that the cut never falls
    // inside the first rank. It is rank-major, so a flat limit below the number
    // of participants would leave everybody past it with no candidate at all —
    // deterministically the same people every round.
    (0 until depth)
      .toVector
      .flatMap(rank => byParticipant.flatMap(_.lift(rank)))
      .take(math.max(Shortlist, byParticipant.size))
      .map: (participant, question, bearing, _) =>
        val answers = sampled.map(ensemble.any(_, participant, bearing))
        (participant, question, answers)

  /**
    * Subdivides each group of worlds according to the answer given in it, so
    * that worlds implying different answers are told apart.
    *
    * Groups already too small to support a reliable comparison are left whole.
    * Without this the search would happily subdivide the ensemble down to
    * single worlds, where the winning option is trivially known and the
    * apparent information is entirely noise.
    *
    * @param groups
    *   The group of each world before subdividing.
    *
    * @param answers
    *   The answer implied by each world.
    *
    * @return
    *   The group of each world after subdividing.
    */
  private def subdivide
    (
      groups: Array[Int],
      answers: Array[Boolean],
    )
    : Array[Int] =
    val sizes    = groups.groupMapReduce(identity)(_ => 1)(_ + _)
    val agreeing = groups
      .indices
      .filter(answers)
      .groupMapReduce(groups)(_ => 1)(_ + _)
      .withDefaultValue(0)

    def divisible(group: Int): Boolean = math.min(
      agreeing(group),
      sizes(group) - agreeing(group),
    ) >= Grain

    Array.tabulate(groups.length): world =>
      val group = groups(world)
      if !divisible(group) then group * 2
      else group * 2 + (if answers(world) then 1 else 0)

  /**
    * Which of the contending options wins in each world under consideration.
    *
    * @param ensemble
    *   The ensemble of sampled worlds.
    *
    * @param sampled
    *   The indices of the worlds under consideration.
    *
    * @param contending
    *   The indices of the options that may be chosen.
    *
    * @return
    *   For each world, the position within `contending` of its winning option.
    */
  private def victors
    (
      ensemble: Ensemble,
      sampled: Array[Int],
      contending: Vector[Int],
    )
    : Array[Int] = sampled
    .zipWithIndex
    .map: (world, index) =>
      val scores  = contending.map(ensemble.score(world, _))
      val highest = scores.max
      val level   = scores.indices.filter(slot => scores(slot) == highest)
      // Ties are shared out across the worlds that hold them rather than always
      // going to the first slot. Awarding them all to one makes a field of
      // evenly matched slots look decided, leaving no uncertainty for a question
      // to resolve and so no question worth asking.
      level(index % level.size)

  /**
    * How uncertain it is which option wins, in bits, averaged over the groups
    * that the answers so far would tell apart.
    *
    * With every world in one group this is simply the entropy of the winner. As
    * answers subdivide the worlds it falls, and how far it falls is exactly the
    * information those answers carry about the decision. It cannot rise, so no
    * question is ever worth less than nothing.
    *
    * @param groups
    *   The group of each world.
    *
    * @param winners
    *   The winning option in each world, as given by [[victors]].
    *
    * @return
    *   The entropy of the winner given the group, in bits.
    */
  private def uncertainty(groups: Array[Int], winners: Array[Int]): Double =
    val sizes = collection.mutable.LongMap.empty[Int]
    val joint = collection.mutable.LongMap.empty[Int]

    var world = 0
    while world < groups.length do
      val group = groups(world).toLong
      sizes(group) = sizes.getOrElse(group, 0) + 1
      val pair = group * MaxContenders + winners(world)
      joint(pair) = joint.getOrElse(pair, 0) + 1
      world += 1

    val total = groups.length.toDouble
    -joint.foldLeft(0.0): (sum, entry) =>
      val (pair, count) = entry
      val share         = count.toDouble / sizes(pair / MaxContenders)
      sum + (count / total) * math.log(share) / math.log(2)
