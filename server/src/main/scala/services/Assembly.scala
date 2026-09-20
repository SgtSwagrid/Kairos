package com.alecdorrington.server
package services

import cats.effect.IO
import com.alecdorrington.server.config.Env
import sttp.tapir.server.metrics.prometheus.PrometheusMetrics
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/**
  * Everything this server serves, gathered from its services.
  *
  * Documentation and metrics are assembled here, once, across every service.
  * Both are served from a fixed path, and the metrics registry rejects the same
  * metric being registered twice, so neither can belong to an individual
  * service.
  */
object Assembly:

  /**
    * The endpoints to serve.
    *
    * @param services
    *   Every service whose endpoints should be served.
    *
    * @return
    *   Every service's endpoints, followed by documentation and metrics
    *   covering all of them.
    */
  def endpoints(services: List[Service]): List[Service.Endpoint] =
    val api = services.flatMap(_.api)
    api ++ documentation(api) :+ metrics

  /**
    * Endpoints used to browse API documentation, generated from the spec using
    * [Swagger](https://swagger.io/).
    *
    * @note
    *   Only available in development mode.
    *
    * @param api
    *   Every endpoint to document.
    *
    * @return
    *   The endpoints serving the documentation.
    */
  private def documentation
    (api: List[Service.Endpoint])
    : List[Service.Endpoint] =
    if !Env.DEV_MODE then List.empty
    else SwaggerInterpreter().fromServerEndpoints(api, Env.NAME, Env.VERSION)

  /**
    * An endpoint that serves metrics about this application, in a format that
    * can be scraped by [Prometheus](https://prometheus.io/).
    */
  private lazy val metrics: Service.Endpoint = PrometheusMetrics
    .default[IO]()
    .metricsEndpoint
