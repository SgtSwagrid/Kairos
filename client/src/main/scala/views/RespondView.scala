package com.alecdorrington.client
package views

import com.alecdorrington.client.components.Display.*
import com.alecdorrington.client.net.Api
import com.alecdorrington.common.api.{Answer, Questionnaire}
import com.alecdorrington.common.model.{Availability, Question}
import com.raquo.laminar.api.L.{*, given}
import scala.scalajs.js.annotation.JSExportTopLevel

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

  /** Whether the latest answers have been sent. */
  private val sent = Var(false)

  /** Whether the questionnaire could not be loaded. */
  private val failed = Var(false)

  override protected def content = div(
    cls("page"),
    loaded --> (form => receive(form)),
    child <--
      questionnaire
        .signal
        .combineWith(failed.signal)
        .map:
          case (_, true)       => missing
          case (None, _)       => div(cls("lede"), "Loading…")
          case (Some(form), _) => sheet(form),
  )

  /** The questionnaire for the participant named in the address. */
  private def loaded: EventStream[Questionnaire] =
    (Route.poll, Route.participant) match
      case (Some(poll), Some(participant)) => Api
          .ask(poll, participant)
          .recover:
            case _ =>
              failed.set(true)
              None
      case _ =>
        failed.set(true)
        EventStream.empty

  /** Takes in a questionnaire from the server, adopting the answers in it. */
  private def receive(form: Questionnaire): Unit =
    questionnaire.set(Some(form))
    chosen.set(
      form.answered.map(answer => answer.question -> answer.availability).toMap,
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
    div(
      cls("masthead"),
      div(
        h1(s"Hello, ${ form.name }"),
        p(cls("lede"), form.title),
      ),
    ),
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
        child.text <--
          sent.signal.map(if _ then "Saved" else "Save my answers"),
        disabled <--
          chosen
            .signal
            .combineWith(sent.signal)
            .map((answers, saved) => answers.isEmpty || saved),
        onClick.mapTo(false) --> sent,
        onClick.flatMapTo(submitted) -->
          (form =>
            receive(form)
            sent.set(true)
          ),
      ),
      child.maybe <--
        sent
          .signal
          .map(saved =>
            Option.when(saved)(span(
              cls("small"),
              cls("faint"),
              "Thank you. You may close this page.",
            )),
          ),
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
  private def submitted: EventStream[Questionnaire] =
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
          .recover:
            case _ => None
      case _ => EventStream.empty

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
                  sent.set(false)
                ),
            ),
            grade.label,
          ),
    ),
  )
