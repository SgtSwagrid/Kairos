package com.alecdorrington.server
package api

import com.alecdorrington.common.api.*
import com.alecdorrington.common.model.*
import com.alecdorrington.common.solve.*
import sttp.tapir.Schema

/**
  * Descriptions of the domain types, for the generated API documentation.
  *
  * These are written out rather than derived wholesale. Identifiers and days
  * are opaque types over primitives and grades are an enumeration of plain
  * names, each of which travels as a string that only its [[io.circe.Codec]]
  * knows about; and declaring the rest explicitly, from the leaves upwards,
  * means a type that cannot be described says so plainly instead of burying the
  * reason inside a derivation several levels deep.
  */
object Schemas:

  given Schema[Day] = Schema.string

  given [Entity] => Schema[Id[Entity]] = Schema.string

  given Schema[Availability] = Schema
    .string
    .description(Availability.all.mkString(", "))

  given [Entity, Value : Schema] => Schema[Map[Id[Entity], Value]] = Schema
    .schemaForMap[Id[Entity], Value](_.value)

  given Schema[Window]      = Schema.derived
  given Schema[Slot]        = Schema.derived
  given Schema[Participant] = Schema.derived
  given Schema[Objective]   = Schema.derived
  given Schema[Question]    = Schema.derived
  given Schema[Response]    = Schema.derived
  given Schema[Pending]     = Schema.derived
  given Schema[Poll]        = Schema.derived

  given Schema[Forecast] = Schema.derived
  given Schema[Verdict]  = Schema.derived
  given Schema[Enquiry]  = Schema.derived
  given Schema[Round]    = Schema.derived
  given Schema[Analysis] = Schema.derived

  given Schema[VenueDraft]       = Schema.derived
  given Schema[ParticipantDraft] = Schema.derived
  given Schema[Draft]            = Schema.derived
  given Schema[Report]           = Schema.derived
  given Schema[Asked]            = Schema.derived
  given Schema[Answer]           = Schema.derived
  given Schema[Questionnaire]    = Schema.derived
