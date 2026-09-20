package com.alecdorrington.common
package model

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

/**
  * A stable string identifier, phantom-tagged by the type of entity that it
  * identifies. The tag makes identifiers of different entities incomparable, so
  * that an [[Id]] of a [[Slot]] can never be mistaken for an [[Id]] of a
  * [[Participant]], at no runtime cost.
  *
  * @tparam Entity
  *   The type of entity being identified.
  */
opaque type Id[Entity] = String

object Id:

  /**
    * Wraps a raw string as an identifier.
    *
    * @param value
    *   The underlying string, which should be unique among entities of this
    *   type.
    *
    * @return
    *   An identifier carrying that string.
    */
  def apply[Entity](value: String): Id[Entity] = value

  extension [Entity](id: Id[Entity])

    /** The underlying string of this identifier. */
    def value: String = id

  given [Entity] => Encoder[Id[Entity]] = Encoder[String].contramap(_.value)

  given [Entity] => Decoder[Id[Entity]] = Decoder[String].map(Id(_))

  given [Entity] => KeyEncoder[Id[Entity]] =
    KeyEncoder[String].contramap(_.value)

  given [Entity] => KeyDecoder[Id[Entity]] = KeyDecoder[String].map(Id(_))

  given [Entity] => Ordering[Id[Entity]] = Ordering.by(_.value)
