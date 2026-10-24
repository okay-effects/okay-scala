package okay.scala3.std

import okay.scala3.*

/** Scala.js's timer: the host's `setTimeout` */
private[std] object Timers:
  def executionContext: scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
  /** `f` after `ms` milliseconds; what is given back cancels it */
  def after(ms: Long)(f: () => Unit): () => Unit =
    val handle = scala.scalajs.js.timers.setTimeout(ms.toDouble)(f())
    () => scala.scalajs.js.timers.clearTimeout(handle)
