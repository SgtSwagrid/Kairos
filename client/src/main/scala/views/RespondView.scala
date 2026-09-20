package com.alecdorrington.client
package views

import com.alecdorrington.client.components.Display.*
import com.alecdorrington.client.net.{Api, Failed}
import com.alecdorrington.common.api.{Answer, Questionnaire}
import com.alecdorrington.common.model.{Availability, Question}
import com.raquo.laminar.api.L.{*, given}
import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * How the last attempt to save the answers went.
  *
  * @param label
  *   What the button should read.
  *
  * @param clickable
  *   Whether pressing it again would do any good.
  */
enum Sending(val label: String, val clickable: Boolean):

  /** Nothing has been sent, or an answer has changed since it was. */
  case Idle extends Sending("Save my answers", true)

  /** A save is in flight. */
  case Saving extends Sending("Saving…", false)

  /** The answers are safely recorded. */
  case Saved extends Sending("Saved", false)

  /** The save did not get through, and may be tried again. */
  case Failed extends Sending("Try again", true)

/**
  * The view on which one participant answers their questions, at URL
  * `/polls/{poll}/ask/{participant}`.
  *
  * The wording here does a good deal of the work. Someone asked whether a date
  * suits them will reasonably assume the date is being held for them, and will
  * either start making plans or refuse to answer until it is certain. Saying
  * plainly that nothing is decided, and offering graded answers rather than yes
  * and no, is what makes it safe to ask early and often.
  */
@JSExportTopLevel("RespondView")
object RespondView extends View:

  /** The questionnaire as the server last described it. */
  private val questionnaire = Var(Option.empty[Questionnaire])

  /** The grade chosen for each question, including those already answered. */
  private val chosen = Var(Map.empty[Question, Availability])

  /** How the last attempt to save went. */
  private val sending = Var(Sending.Idle)

  /** Why the questionnaire could not be loaded, if it could not. */
  private val failed = Var(Option.empty[Failed])

  override protected def content = div(
    cls("page"),
    loaded --> (form => receive(form)),
    child <--
      questionnaire
        .signal
        .combineWith(failed.signal)
        .map:
          case (_, Some(reason)) if reason.absent => missing
          case (None, Some(_))                    => unreachable
          case (None, None)    => div(cls("lede"), "Loading…")
          case (Some(form), _) => sheet(form),
  )

  /** The questionnaire for the participant named in the address. */
  private def loaded: EventStream[Questionnaire] =
    (Route.poll, Route.participant) match
      case (Some(poll), Some(participant)) => Api
          .ask(poll, participant)
          .recover:
            case reason: Failed =>
              failed.set(Some(reason))
              None
            case _ =>
              failed.set(Some(Failed(0, "ask")))
              None
      case _ =>
        failed.set(Some(Failed(404, "ask")))
        EventStream.empty

  /** Takes in a questionnaire from the server, adopting the answers in it. */
  private def receive(form: Questionnaire): Unit =
    failed.set(None)
    questionnaire.set(Some(form))
    chosen.set(
      form.answered.map(answer => answer.question -> answer.availability).toMap,
    )

  /** The page shown when the server could not be reached at all. */
  private def unreachable: HtmlElement = div(
    h1("Could not reach the organiser"),
    p(
      cls("lede"),
      "Your link is probably fine; the request did not get through. Please " +
        "try again in a moment.",
    ),
  )

  /** The page shown when the link does not correspond to anybody. */
  private def missing: HtmlElement = div(
    h1("This link does not work"),
    p(
      cls("lede"),
      "It may have been mistyped, or the event it belonged to may have been " +
        "cancelled. Ask whoever sent it to you for a new one.",
    ),
  )

  /** The questionnaire itself. */
  private def sheet(form: Questionnaire): HtmlElement = div(
    masthead(s"Hello, ${ form.name }", form.title)(),
    div(
      cls("stack"),
      div(
        cls("note"),
        strong("Nothing here is a booking, and no date is being held."),
        " We are narrowing down when and where to hold this, and your answers " +
          "help us pick something that suits as many people as possible. " +
          "Answer roughly; you can change any of it later, and we will confirm " +
          "properly once a date is settled.",
      ),
      if form.questions.isEmpty then nothingToAsk else questions(form),
    ),
  )

  /** The panel shown when this participant has not been asked anything. */
  private def nothingToAsk: HtmlElement =
    panel("Nothing to answer just yet", "")(p(
      cls("small"),
      "There are no questions waiting for you. If you were expecting some, " +
        "check back shortly, or ask the organiser to send another round.",
    ))

  /** The questions, with the means of answering and sending them. */
  private def questions(form: Questionnaire): HtmlElement = panel(
    if form.answered.isEmpty then "A few questions" else "Your answers so far",
    "Pick whichever answer is closest. There are no wrong answers, and " +
      "\"could go either way\" is a genuinely useful one.",
  )(
    form.questions.map(asked => question(asked.question, asked.prompt)),
    div(
      cls("row"),
      marginTop("18px"),
      button(
        child.text <-- sending.signal.map(_.label),
        // Disabled while a save is in flight as well as once it is done: two
        // clicks would send two sets of answers and two writes to the store.
        disabled <--
          chosen
            .signal
            .combineWith(sending.signal)
            .map((answers, state) => answers.isEmpty || !state.clickable),
        onClick.mapTo(Sending.Saving) --> sending,
        onClick.flatMapTo(submitted) -->
          (outcome =>
            outcome.foreach(receive)
            sending.set(outcome.fold(Sending.Failed)(_ => Sending.Saved))
          ),
      ),
      child.maybe <--
        sending
          .signal
          .map:
            case Sending.Saved => Some(span(
                cls("small"),
                cls("faint"),
                "Thank you. You may close this page.",
              ))
            case Sending.Failed => Some(span(
                cls("small"),
                color("var(--bad)"),
                "That did not save. Please try again; nothing has been " +
                  "recorded.",
              ))
            case _ => None,
      child.maybe <--
        chosen
          .signal
          .map(answers =>
            Option.when(answers.sizeIs < form.questions.size)(span(
              cls("small"),
              cls("faint"),
              s"${ form.questions.size - answers.size } still unanswered.",
            )),
          ),
    ),
  )

  /** Sends whatever has been chosen, for the participant in the address. */
  private def submitted: EventStream[Option[Questionnaire]] =
    (Route.poll, Route.participant) match
      case (Some(poll), Some(participant)) => Api
          .answer(
            poll,
            participant,
            chosen
              .now()
              .toList
              .map((question, grade) => Answer(question, grade)),
          )
          .map(Option(_))
          .recover { case _ => Some(None) }
      case _ => EventStream.fromValue(None)

  /** One question, with a graded answer to choose from. */
  private def question(subject: Question, prompt: String): HtmlElement = div(
    cls("question"),
    div(cls("prompt"), prompt),
    div(
      cls("choices"),
      Availability
        .all
        .map: grade =>
          label(
            cls("choice"),
            cls.toggle("chosen") <--
              chosen.signal.map(_.get(subject).contains(grade)),
            input(
              typ("radio"),
              nameAttr(prompt),
              onChange.mapTo(grade) -->
                (picked =>
                  chosen.update(_.updated(subject, picked))
                  sending.set(Sending.Idle)
                ),
            ),
            grade.label,
          ),
    ),
  )
