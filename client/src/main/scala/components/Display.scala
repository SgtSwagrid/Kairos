package com.alecdorrington.client
package components

import com.raquo.laminar.api.L.{*, given}

/** Small pieces of presentation shared between the views. */
object Display:

  /** A proportion rendered as a whole percentage. */
  def percent(fraction: Double): String = f"${ 100 * fraction }%.0f%%"

  /** A number rendered to one decimal place. */
  def decimal(value: Double): String = f"$value%.1f"

  /** A small number rendered with enough places to tell it from its neighbours. */
  def precise(value: Double): String = f"$value%.3f"

  /** A number rendered without decimals, with thousands separated. */
  def whole(value: Double): String =
    val rounded = math.round(value).toString
    rounded.reverse.grouped(3).mkString(",").reverse.replace(",-", "-")

  /**
    * A headline figure with its label beneath. Named for what it shows rather
    * than for the element, since Laminar already binds `figure` to the tag.
    *
    * @param value
    *   The figure itself, already formatted.
    *
    * @param label
    *   What the figure measures.
    *
    * @return
    *   An element displaying the figure.
    */
  def statistic(value: String, label: String): HtmlElement = div(
    cls("figure"),
    div(cls("value"), value),
    div(cls("label"), label),
  )

  /**
    * A horizontal bar showing a proportion.
    *
    * @param fraction
    *   How full the bar should be, clamped between none and full.
    *
    * @param tone
    *   A modifier class colouring the bar, such as `good` or `bad`.
    *
    * @return
    *   An element displaying the bar.
    */
  def bar(fraction: Double, tone: String = ""): HtmlElement = div(
    cls("bar"),
    cls(tone),
    span(width(s"${ 100 * math.max(0.0, math.min(1.0, fraction)) }%")),
  )

  /**
    * A titled panel.
    *
    * @param heading
    *   The panel's title.
    *
    * @param lede
    *   A sentence explaining what the panel shows.
    *
    * @param contents
    *   The body of the panel.
    *
    * @return
    *   An element displaying the panel.
    */
  def panel
    (heading: String, lede: String)
    (contents: Modifier[HtmlElement]*)
    : HtmlElement = div(
    cls("panel"),
    h2(heading),
    Option.when(lede.nonEmpty)(p(cls("lede"), cls("small"), lede)),
    contents,
  )

  /** A small pill of text, optionally toned. */
  def badge(text: String, tone: String = ""): HtmlElement =
    span(cls("badge"), cls(tone), text)

  /** A tone for a bar or badge, by how alarming the proportion is. */
  def risk(fraction: Double): String =
    if fraction >= 0.5 then "bad"
    else if fraction >= 0.15 then "warn"
    else "good"
