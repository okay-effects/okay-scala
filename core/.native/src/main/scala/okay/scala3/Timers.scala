package okay.scala3

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

/** Scala Native's timer, as the JVM's: one daemon thread, its callbacks short — they resume a program on its execution context */
private[scala3] object Timers:
  private lazy val scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor { r =>
    val t = Thread(r, "okay-scala-timer")
    t.setDaemon(true)
    t
  }
  def executionContext: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  /** `f` after `ms` milliseconds; what is given back cancels it */
  def after(ms: Long)(f: () => Unit): () => Unit =
    val scheduled = scheduler.schedule((() => f()): Runnable, ms, TimeUnit.MILLISECONDS)
    () => scheduled.cancel(false)
