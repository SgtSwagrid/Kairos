package com.alecdorrington.server
package services

import cats.effect.IO
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.server.ServerEndpoint

/**
  * The base trait for all API services in this application. Each service is a
  * thematic grouping of API endpoint implementations.
  *
  * A service provides only its own endpoints. Documentation and metrics are
  * assembled once across every service by [[Assembly]] rather than by each
  * service for itself, because both are served from a fixed path and registered
  * against a shared registry; a service that provided its own would collide
  * with the next one, and only the first would be reachable.
  *
  * @param serviceName
  *   The name for this service, used in API documentation.
  */
trait Service(val serviceName: String):

  /** A collection of all endpoints implemented in this service. */
  def api: List[Service.Endpoint]

object Service:

  /** The kinds of capabilities that API endpoints in this application have. */
  type Capabilities = WebSockets & Fs2Streams[IO]

  /** The type of all API endpoints in this application. */
  type Endpoint = ServerEndpoint[Capabilities, IO]
