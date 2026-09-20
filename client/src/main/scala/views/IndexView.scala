package com.alecdorrington.client
package views

import com.alecdorrington.client.components.Display.*
import com.alecdorrington.client.net.{Api, Recent}
import com.alecdorrington.common.api.Report
import com.raquo.laminar.api.L.{*, given}
import scala.scalajs.js.annotation.JSExportTopLevel

/** The view for the index page of the website, at URL `/`. */
@JSExportTopLevel("IndexView")
object IndexView extends View:

  /** The polls this browser has opened before. */
  private val existing = Var(Recent.all)

  /** Whether a poll is being created, so the button can be disabled. */
  private val creating = Var(false)

  override protected def content = div(
    cls("page"),
    div(
      cls("masthead"),
      div(
        h1("Kairos"),
        p(
          cls("lede"),
          "Choose when and where to hold something, by asking the people " +
            "invited as little as possible.",
        ),
      ),
    ),
    div(
      cls("stack"),
      method,
      start,
      child.maybe <--
        existing.signal.map(polls => Option.when(polls.nonEmpty)(saved(polls))),
    ),
  )

  /** A newly created example poll, with failure leaving the button usable. */
  private def created: EventStream[Report] = Api
    .example
    .map: report =>
      Recent.remember(Recent(
        report.poll.id.value,
        report.poll.title,
      ))
      report
    .recover:
      case _ =>
        creating.set(false)
        None

  /** An explanation of how the tool goes about its work. */
  private def method: HtmlElement = panel(
    "How it works",
    "Availability is expensive to ask for and cheap to model.",
  )(div(
    cls("grid"),
    step(
      "1",
      "Describe the options",
      "Name the venues, when each is free, what it holds and what it costs. " +
        "Kairos works out every placement the event could take.",
    ),
    step(
      "2",
      "Ask a little",
      "It picks the few questions whose answers would most change the " +
        "decision, and prefers asking about broad periods over naming dates, " +
        "so that nobody is asked to hold a weekend that may not happen.",
    ),
    step(
      "3",
      "Model the rest",
      "Answers become a probability that each person attends each option, " +
        "and thousands of simulated turnouts give the chance that each " +
        "option is in fact the best one.",
    ),
    step(
      "4",
      "Stop when asking stops paying",
      "It reports what perfect knowledge of everyone's diary would be worth. " +
        "Once that falls below the cost of another round, book.",
    ),
  ))

  /** One numbered step of the explanation. */
  private def step
    (
      number: String,
      heading: String,
      body: String,
    )
    : HtmlElement = div(
    div(cls("badge"), number),
    h3(heading, marginTop("8px")),
    p(cls("small"), cls("faint"), body),
  )

  /** The means of starting a new poll. */
  private def start: HtmlElement = panel(
    "Try it",
    "A worked example: three mountain huts of differing size, cost and " +
      "availability, and thirty-two guests who have said nothing yet.",
  )(div(
    cls("row"),
    button(
      "Create a worked example",
      disabled <-- creating.signal,
      onClick.mapTo(true) --> creating,
      onClick.flatMapTo(created) -->
        (report => Route.go(Route.organiser(report.poll.id.value))),
    ),
    span(
      cls("small"),
      cls("faint"),
      "Nothing is sent to anybody; the guests are invented.",
    ),
  ))

  /** The polls already on this server. */
  private def saved(polls: List[Recent]): HtmlElement = panel(
    "Polls you have opened",
    "Kept in this browser alone. A poll's link is what keeps it private, so " +
      "the server will not list them; if you lose a link, it is lost.",
  )(table(
    thead(tr(th("Event"), th("Link"))),
    tbody(
      polls.map: poll =>
        tr(
          td(a(
            href(Route.organiser(poll.id)),
            poll.title,
          )),
          td(button(
            cls("quiet"),
            cls("tiny"),
            padding("2px 8px"),
            "forget",
            onClick -->
              (_ =>
                Recent.forget(poll.id)
                existing.set(Recent.all)
              ),
          )),
        ),
    ),
  ))
