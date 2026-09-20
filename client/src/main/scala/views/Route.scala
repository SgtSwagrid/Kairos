package com.alecdorrington.client
package views

import org.scalajs.dom

/**
  * The parts of the current address that a view needs.
  *
  * Views are chosen by the server, which injects the name of the view into the
  * page, but it does not pass along which poll or participant the page is for.
  * Reading that back off the address keeps the page template free of
  * page-specific detail.
  */
object Route:

  /** The path of the current address, split on slashes, with blanks removed. */
  private def segments: List[String] = dom
    .window
    .location
    .pathname
    .split('/')
    .filter(_.nonEmpty)
    .toList

  /** The identifier of the poll this page is for, if the address names one. */
  def poll: Option[String] = segments match
    case "polls" :: id :: _ => Some(id)
    case _                  => None

  /** The identifier of the participant this page is for, if there is one. */
  def participant: Option[String] = segments match
    case "polls" :: _ :: "ask" :: who :: _ => Some(who)
    case _                                 => None

  /** The address of the organiser's page for the given poll. */
  def organiser(poll: String): String = s"/polls/$poll"

  /** The full address at which the given participant answers their questions. */
  def answering(poll: String, participant: String): String =
    s"${ dom.window.location.origin }/polls/$poll/ask/$participant"

  /** Sends the browser to the given address. */
  def go(url: String): Unit = dom.window.location.href = url
