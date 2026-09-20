package com.alecdorrington.client
package net

import io.circe.{Codec, Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom
import scala.util.Try

/**
  * A poll this browser has opened before.
  *
  * @param id
  *   The poll's identifier, which is also the only thing keeping it private.
  *
  * @param title
  *   What the poll is called, so the list means something.
  */
final case class Recent(id: String, title: String) derives Codec.AsObject

object Recent:

  /** Where the list is kept in the browser. */
  private val Store = "kairos.recent"

  /** How many polls to remember. */
  private val Keep = 12

  /**
    * The polls this browser has opened, most recent first.
    *
    * Kept here rather than asked of the server, and deliberately. The server
    * once offered a list of every poll it held, which the landing page showed:
    * that made the identifiers useless as a means of keeping a poll private,
    * since anybody arriving at the site was handed all of them along with their
    * guest lists and answers. A poll's link is its access control, so only the
    * browser that made it has any business remembering it.
    *
    * @return
    *   The remembered polls, or none if the browser will not say.
    */
  def all: List[Recent] = read.getOrElse(List.empty)

  /**
    * Remembers a poll, moving it to the front if it is already known.
    *
    * @param poll
    *   The poll to remember.
    */
  def remember(poll: Recent): Unit =
    write(poll :: all.filterNot(_.id == poll.id).take(Keep - 1))

  /**
    * Forgets a poll, without deleting anything on the server.
    *
    * @param id
    *   The identifier of the poll to forget.
    */
  def forget(id: String): Unit = write(all.filterNot(_.id == id))

  /** Reads the list, treating any failure as an empty one. */
  private def read: Option[List[Recent]] = Try(
    Option(dom.window.localStorage.getItem(Store)),
  ).toOption.flatten.flatMap(decode[List[Recent]](_).toOption)

  /** Writes the list, ignoring a browser that refuses to store it. */
  private def write(polls: List[Recent]): Unit = Try(
    dom.window.localStorage.setItem(Store, polls.asJson.noSpaces),
  ).toOption.getOrElse(())
