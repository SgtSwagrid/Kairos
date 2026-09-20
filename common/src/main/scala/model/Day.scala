package com.alecdorrington.common
package model

import io.circe.{Decoder, Encoder}
import scala.util.Try

/**
  * A single calendar day, represented as a count of days since the Unix epoch
  * (`1970-01-01`). Deliberately independent of `java.time`, which has no
  * Scala.js implementation, so that the same arithmetic is available on the
  * server and in the browser.
  */
opaque type Day = Int

object Day:

  /** The number of days in each four-century era of the Gregorian calendar. */
  private val DaysPerEra = 146097

  /** The offset in days from the start of an era to the Unix epoch. */
  private val EpochOffset = 719468

  /** Names of the twelve months, indexed from January at `1`. */
  private val MonthNames = Vector(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
  )

  /** Abbreviated names of the seven weekdays, indexed from Sunday at `0`. */
  private val WeekdayNames = Vector(
    "Sun",
    "Mon",
    "Tue",
    "Wed",
    "Thu",
    "Fri",
    "Sat",
  )

  /**
    * The day at a given offset from the epoch.
    *
    * @param epochDay
    *   The number of days since `1970-01-01`, which may be negative.
    *
    * @return
    *   The corresponding day.
    */
  def ofEpochDay(epochDay: Int): Day = epochDay

  /**
    * Constructs a day from its calendar components, using Howard Hinnant's
    * `days_from_civil` algorithm. No validation is performed, so out-of-range
    * components are silently normalised.
    *
    * @param year
    *   The proleptic Gregorian year.
    *
    * @param month
    *   The month, from `1` for January to `12` for December.
    *
    * @param day
    *   The day of the month, from `1`.
    *
    * @return
    *   The day denoted by those components.
    */
  def of(year: Int, month: Int, day: Int): Day =
    val shifted   = if month <= 2 then year - 1 else year
    val era       = (if shifted >= 0 then shifted else shifted - 399) / 400
    val yearOfEra = shifted - era * 400
    val monthTerm = if month > 2 then month - 3 else month + 9
    val dayOfYear = (153 * monthTerm + 2) / 5 + day - 1
    val dayOfEra = yearOfEra * 365 + yearOfEra / 4 - yearOfEra / 100 + dayOfYear
    era * DaysPerEra + dayOfEra - EpochOffset

  /**
    * Parses a day from an ISO-8601 date, as produced by [[Day.iso]].
    *
    * @param text
    *   A date of the form `yyyy-mm-dd`.
    *
    * @return
    *   The parsed day, or [[None]] if the text is not a valid ISO date.
    */
  def parse(text: String): Option[Day] = text.split('-') match
    case Array(year, month, day) => Try(of(year.toInt, month.toInt, day.toInt))
        .toOption
    case _ => None

  extension (day: Day)

    /** The number of days from the Unix epoch to this day. */
    def epochDay: Int = day

    /**
      * The calendar components of this day, using Howard Hinnant's
      * `civil_from_days` algorithm.
      *
      * @return
      *   A triple of the year, the month from `1`, and the day of month from
      *   `1`.
      */
    def civil: (Int, Int, Int) =
      val shifted = day + EpochOffset
      val era = (if shifted >= 0 then shifted else shifted - (DaysPerEra - 1)) /
        DaysPerEra
      val dayOfEra  = shifted - era * DaysPerEra
      val yearOfEra =
        (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 -
          dayOfEra / (DaysPerEra - 1)) / 365
      val dayOfYear = dayOfEra -
        (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100)
      val monthTerm  = (5 * dayOfYear + 2) / 153
      val dayOfMonth = dayOfYear - (153 * monthTerm + 2) / 5 + 1
      val month      = if monthTerm < 10 then monthTerm + 3 else monthTerm - 9
      val year       = yearOfEra + era * 400 + (if month <= 2 then 1 else 0)
      (year, month, dayOfMonth)

    /** The proleptic Gregorian year containing this day. */
    def year: Int = day.civil._1

    /** The month containing this day, from `1` for January. */
    def month: Int = day.civil._2

    /** The day of the month, from `1`. */
    def dayOfMonth: Int = day.civil._3

    /** The day of the week, from `0` for Sunday to `6` for Saturday. */
    def weekday: Int = math.floorMod(day + 4, 7)

    /** The day that falls the given number of days after this one. */
    def plus(days: Int): Day = day + days

    /** The number of days from this day to the given later day. */
    def until(other: Day): Int = other - day

    /** This day as an ISO-8601 date of the form `yyyy-mm-dd`. */
    def iso: String =
      val (year, month, dayOfMonth) = day.civil
      f"$year%04d-$month%02d-$dayOfMonth%02d"

    /** The full name of the month containing this day. */
    def monthName: String = MonthNames(day.month - 1)

    /** The abbreviated name of this day's weekday. */
    def weekdayName: String = WeekdayNames(day.weekday)

    /** This day rendered for display, as in `Fri 11 Jun 2027`. */
    def show: String =
      val (year, _, dayOfMonth) = day.civil
      s"${ day.weekdayName } $dayOfMonth ${ day.monthName.take(3) } $year"

    /** The first day of the month containing this day. */
    def startOfMonth: Day =
      val (year, month, _) = day.civil
      of(year, month, 1)

    /** The last day of the month containing this day. */
    def endOfMonth: Day =
      val (year, month, _)      = day.civil
      val (nextYear, nextMonth) =
        if month == 12 then (year + 1, 1) else (year, month + 1)
      of(nextYear, nextMonth, 1).plus(-1)

  given Ordering[Day] = Ordering.by(_.epochDay)

  given Encoder[Day] = Encoder[String].contramap(_.iso)

  given Decoder[Day] = Decoder[String].emap: text =>
    parse(text).toRight(s"'$text' is not an ISO-8601 date.")
