package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * Something a participant can be asked. Questions come in two resolutions,
  * which trade breadth against precision.
  *
  * A [[Question.AboutWindow]] is a pooled question in the sense of group
  * testing: one answer bears on every slot inside the window. It is cheap and
  * wide, and a negative answer is conclusive, which is what makes screening
  * rounds so efficient. A positive answer is weaker, because being free
  * somewhere in a month says less about one particular weekend in it.
  *
  * A [[Question.AboutSlot]] is precise but narrow, and is worth asking only
  * once the field has been reduced to slots that are genuinely still in
  * contention.
  */
enum Question derives Codec.AsObject:

  /** Asks whether the participant could attend one particular slot. */
  case AboutSlot(slot: Id[Slot])

  /** Asks whether the participant could attend at some point in a window. */
  case AboutWindow(window: Window)

  /**
    * Whether an answer to this question carries information about the given
    * slot. A window question bears on a slot only if it wholly encloses it: a
    * partial overlap would leave the answer ambiguous, which is not worth the
    * modelling risk.
    *
    * @param slot
    *   The slot in question.
    *
    * @return
    *   `true` if answering this question tells us something about `slot`.
    */
  def bearsOn(slot: Slot): Boolean = this match
    case AboutSlot(id)       => id == slot.id
    case AboutWindow(window) => window.encloses(slot.window)

  /**
    * The question as it should be put to a participant. Window questions are
    * phrased as being about an event of the given length falling somewhere in
    * the window, rather than about the whole window, so that the answer means
    * what the solver assumes it means.
    *
    * @param slots
    *   Every slot in the poll, indexed by identifier, used to describe slot
    *   questions.
    *
    * @param length
    *   The number of days the event occupies.
    *
    * @return
    *   A prompt to show the participant.
    */
  def prompt(slots: Map[Id[Slot], Slot], length: Int): String = this match
    case AboutSlot(id) => slots
        .get(id)
        .map(slot => s"${ slot.window.show }, at ${ slot.venue }")
        .getOrElse("An unknown date")
    case AboutWindow(window) =>
      s"Some $length days during ${ window.showBrief }"

object Question:

  /**
    * The questions worth considering for a poll, being every slot and every
    * calendar month that the slots touch.
    *
    * Offering both resolutions to the solver is what lets it choose its own
    * strategy: with no answers in hand the wide monthly questions carry far
    * more information per question and will be selected, and only once the
    * field narrows do individual slots become worth asking about.
    *
    * @param slots
    *   Every slot in the poll.
    *
    * @return
    *   Every candidate question, coarse questions first.
    */
  def candidates(slots: Seq[Slot]): List[Question] =
    val months = Window
      .enclosing(slots.map(_.window))
      .toList
      .flatMap(Window.months)

    // A slot lying across a month boundary is enclosed by no month, and a
    // question only bears on what it encloses. Left as it is, such a slot is
    // never screened: everything around it is talked down to near-certainty
    // while it keeps the prior, and it then rises to the top of the ranking for
    // no better reason than that nobody was asked about it. Each one therefore
    // gets a window of its own, spanning the months it touches.
    val bridges = slots
      .filterNot(slot => months.exists(_.encloses(slot.window)))
      .flatMap(slot => Window.enclosing(months.filter(_.overlaps(slot.window))))
      .distinct

    val pooled = (months ++ bridges).filter(window =>
      slots.exists(slot => window.encloses(slot.window)),
    )
    pooled.map(AboutWindow(_)).toList ++
      slots.map(slot => AboutSlot(slot.id)).toList
