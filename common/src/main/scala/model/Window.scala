package com.alecdorrington.common
package model

import io.circe.Codec

/**
  * A contiguous, inclusive range of calendar days.
  *
  * @param start
  *   The first day of the window.
  *
  * @param end
  *   The last day of the window, which should not precede [[start]].
  */
final case class Window(start: Day, end: Day) derives Codec.AsObject:

  /** The number of days spanned by this window, at least `1`. */
  def length: Int = math.max(1, start.until(end) + 1)

  /** Whether the given day falls within this window. */
  def contains(day: Day): Boolean = start.epochDay <= day.epochDay &&
    day.epochDay <= end.epochDay

  /** Whether this window wholly encloses the given window. */
  def encloses(other: Window): Boolean = contains(other.start) &&
    contains(other.end)

  /** Whether this window shares at least one day with the given window. */
  def overlaps(other: Window): Boolean = contains(other.start) ||
    other.contains(start)

  /** The smallest window enclosing both this and the given window. */
  def union(other: Window): Window = Window(
    Day.ofEpochDay(math.min(start.epochDay, other.start.epochDay)),
    Day.ofEpochDay(math.max(end.epochDay, other.end.epochDay)),
  )

  /**
    * This window rendered for display, as a single day where the window spans
    * one day, and otherwise as a range.
    */
  def show: String =
    if start.epochDay == end.epochDay then start.show
    else s"${ start.show } – ${ end.show }"

  /** A terse rendering of this window, naming the month where possible. */
  def showBrief: String =
    if start == start.startOfMonth && end == end.endOfMonth then
      s"${ start.monthName } ${ start.year }"
    else
      s"${ start.dayOfMonth } ${ start.monthName.take(3) } – " +
        s"${ end.dayOfMonth } ${ end.monthName.take(3) }"

object Window:

  /**
    * A window of the given length beginning on the given day.
    *
    * @param start
    *   The first day of the window.
    *
    * @param length
    *   The number of days spanned, at least `1`.
    *
    * @return
    *   A window covering `length` days from `start`.
    */
  def of(start: Day, length: Int): Window = Window(
    start,
    start.plus(math.max(1, length) - 1),
  )

  /**
    * The whole-month windows that intersect the given window, clipped to it.
    * Used to bucket a span of candidate dates into coarse periods suitable for
    * screening questions.
    *
    * @param span
    *   The window to divide into months.
    *
    * @return
    *   One window per calendar month touched by `span`, in chronological order.
    */
  def months(span: Window): List[Window] = List.unfold(span.start): day =>
    Option.when(span.contains(day)):
      val month = Window(
        Day.ofEpochDay(math.max(
          day.startOfMonth.epochDay,
          span.start.epochDay,
        )),
        Day.ofEpochDay(math.min(
          day.endOfMonth.epochDay,
          span.end.epochDay,
        )),
      )
      (month, day.endOfMonth.plus(1))

  /** The smallest window enclosing all of the given windows. */
  def enclosing(windows: Seq[Window]): Option[Window] =
    windows.reduceOption(_.union(_))
