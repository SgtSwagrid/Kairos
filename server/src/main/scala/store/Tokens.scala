package com.alecdorrington.server
package store

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.common.model.Id
import java.security.SecureRandom
import java.util.HexFormat

/**
  * Identifiers that double as the only thing keeping a poll private.
  *
  * Kairos has no accounts. Whoever holds the link to a poll can see its advice,
  * and whoever holds a participant's link can answer as them, so those links are
  * the access control and the identifiers in them have to be unguessable. Short
  * or sequential identifiers would not merely be untidy: with a poll's link in
  * hand, participants numbered `p0` upwards can be walked through one at a time
  * to read the whole guest list and overwrite anybody's answers.
  */
object Tokens:

  /** How many random bytes each identifier carries. */
  private val Length: Int = 16

  /** The source of randomness, seeded by the platform rather than by a clock. */
  private val random: SecureRandom = SecureRandom()

  /**
    * A fresh unguessable identifier.
    *
    * @tparam Entity
    *   The type of entity being identified.
    *
    * @return
    *   An identifier of [[Length]] random bytes, in hexadecimal.
    */
  def next[Entity]: IO[Id[Entity]] = IO:
    val bytes = new Array[Byte](Length)
    random.nextBytes(bytes)
    Id[Entity](HexFormat.of().formatHex(bytes))

  /**
    * A number of fresh unguessable identifiers.
    *
    * @param count
    *   How many to generate.
    *
    * @return
    *   That many distinct identifiers.
    */
  def several[Entity](count: Int): IO[List[Id[Entity]]] =
    List.fill(count)(()).traverse(_ => next[Entity])
