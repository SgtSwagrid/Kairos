package com.alecdorrington.client
package dev

import org.scalajs.dom
import org.scalajs.dom.WebSocket
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("HotReload")
object HotReload:

  /**
    * Initialise automatic reloading. When the server is restarted (detected
    * through WebSocket disconnection), the client will repeatedly attempt to
    * reconnect until it is successful. For use during development.
    */
  @JSExport
  def enable(): Unit =

    val location = dom.window.location
    val protocol = if location.protocol == "https:" then "wss" else "ws"
    val url      = s"$protocol://${ location.host }/hot-reload"

    // Reloading at once would end the page, so the retry that followed it could
    // never run; if the server were still down the browser simply showed an
    // error. Waiting first and reloading on the next attempt means the page
    // sits still until there is something to come back to.
    def listen(): Unit = WebSocket(url).onclose =
      _ => dom.window.setTimeout(() => reconnect(), 500)

    def reconnect(): Unit =
      val socket = WebSocket(url)
      socket.onopen = _ => location.reload()
      socket.onclose = _ => dom.window.setTimeout(() => reconnect(), 500)

    listen()
