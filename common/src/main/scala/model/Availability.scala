package com.alecdorrington.common
package model

import io.circe.{Decoder, Encoder}

/**
  * A participant's graded answer about their ability to attend. Grades are
  * deliberately coarse and hedged, because asking for a commitment invites
  * either refusal or a false promise, whereas asking for a likelihood is cheap
  * to answer honestly.
  *
  * Each grade carries the probability that the participant does in fact attend.
  * No grade is certain: even a firm `Yes` may be broken, and a firm `No` may be
  * reconsidered, so treating either as absolute would make the model
  * overconfident and starve later rounds of information.
  *
  * @param probability
  *   The probability that a participant giving this answer attends.
  *
  * @param label
  *   A short description of this grade, for display to participants.
  */
enum Availability(val probability: Double, val label: String):

  /** The participant expects to attend. */
  case Yes extends Availability(0.95, "Works for me")

  /** The participant expects to attend, but sees some risk. */
  case Probably extends Availability(0.78, "Probably workable")

  /** The participant genuinely does not know. */
  case Unsure extends Availability(0.5, "Could go either way")

  /** The participant expects not to attend, but it is not ruled out. */
  case ProbablyNot extends Availability(0.22, "Would be difficult")

  /** The participant does not expect to attend. */
  case No extends Availability(0.03, "Not possible")

object Availability:

  /** Every grade, from most to least available. */
  val all: List[Availability] = values.toList

  /**
    * The grade of the given name.
    *
    * @param name
    *   The name of the grade, as produced by `toString`.
    *
    * @return
    *   The matching grade, or [[None]] if the name is not recognised.
    */
  def parse(name: String): Option[Availability] = all.find(_.toString == name)

  given Encoder[Availability] = Encoder[String].contramap(_.toString)

  given Decoder[Availability] = Decoder[String].emap: name =>
    parse(name).toRight(s"'$name' is not an availability grade.")
