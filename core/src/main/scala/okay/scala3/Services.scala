package okay.scala3

/*
 * SERVICES AS EFFECTS: numbers, randomness, storage, time, logging — each an effect whose handler decides what it
 * is: a counter or a database's sequence, a seed or the system's generator, a map or a store, a virtual clock or the
 * system's, a list or a sink. The program is the same; a test gives it handlers that are always the same.
 */

/** a supply of fresh numbers, each never given before in the run */
enum Fresh[X]:
  case Next() extends Fresh[Long]

object Fresh:
  def next[R[_]](using In[Fresh, R]): Long ! R = perform[Fresh](Next())

  /** numbers from `start` on */
  def from[R[_], Rest[_], A](start: Long)(p: A ! R)(using Split[Fresh, R, Rest]): A ! Rest =
    handle(p, Handler.stateful[Fresh, Rest, A, A, Long](start)((_, a) => pure(a))(
      [X] => (op: Fresh[X], cell: Cell[Long]) => op match
        case Next() =>
          val n = cell.get
          cell.set(n + 1)
          n))

/** random numbers: the handler's generator — a seed's, so a run is the same each time, and a `k` resumed twice
 * resumes the generator of its time */
enum Random[X]:
  case NextLong() extends Random[Long]

object Random:
  def long[R[_]](using In[Random, R]): Long ! R = perform[Random](NextLong())
  /** a number in `[0, bound)`, `bound` positive */
  def int[R[_]](bound: Int)(using In[Random, R]): Int ! R = long[R].map(n => java.lang.Math.floorMod(n, bound.toLong).toInt)
  /** a number in `[0, 1)` */
  def double[R[_]](using In[Random, R]): Double ! R = long[R].map(n => (n >>> 11) * 1.1102230246251565e-16)

  /** numbers from `seed`, by splitmix64 */
  def seeded[R[_], Rest[_], A](seed: Long)(p: A ! R)(using Split[Random, R, Rest]): A ! Rest =
    handle(p, Handler.stateful[Random, Rest, A, A, Long](seed)((_, a) => pure(a))(
      [X] => (op: Random[X], cell: Cell[Long]) => op match
        case NextLong() =>
          val s = cell.get + 0x9e3779b97f4a7c15L
          cell.set(s)
          var z = s
          z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
          z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
          z ^ (z >>> 31)))

/** a store of values `V` by key `K`; in a row, `Store.Of[K, V]` */
enum Store[K, V, X]:
  case Load[K, V](key: K) extends Store[K, V, Option[V]]
  case Save[K, V](key: K, value: V) extends Store[K, V, Unit]
  case Remove[K, V](key: K) extends Store[K, V, Unit]

object Store:
  /** the effect in a row: `Reader % String + Store.Of[String, Int]` */
  type Of[K, V] = [X] =>> Store[K, V, X]

  def load[K, V, R[_]](key: K)(using In[Of[K, V], R]): Option[V] ! R = perform[Of[K, V]](Load(key))
  def save[K, V, R[_]](key: K, value: V)(using In[Of[K, V], R]): Unit ! R = perform[Of[K, V]](Save(key, value))
  def remove[K, V, R[_]](key: K)(using In[Of[K, V], R]): Unit ! R = perform[Of[K, V]](Remove[K, V](key))

  /** a map in memory from `initial`: the map it ends with, and the value */
  def inMemory[K, V, R[_], Rest[_], A](initial: Map[K, V])(p: A ! R)(using Split[Of[K, V], R, Rest])
    : (Map[K, V], A) ! Rest =
    handle(p, Handler.stateful[Of[K, V], Rest, A, (Map[K, V], A), Map[K, V]](initial)((m, a) => pure((m, a)))(
      [X] => (op: Store[K, V, X], cell: Cell[Map[K, V]]) => op match
        case Load(key) => cell.get.get(key)
        case Save(key, value) => cell.set(cell.get.updated(key, value))
        case Remove(key) => cell.set(cell.get - key)))

/** time, in milliseconds: what it is now, and waiting */
enum Clock[X]:
  case Now() extends Clock[Long]
  case Sleep(millis: Long) extends Clock[Unit]

object Clock:
  def now[R[_]](using In[Clock, R]): Long ! R = perform[Clock](Now())
  def sleep[R[_]](millis: Long)(using In[Clock, R]): Unit ! R = perform[Clock](Sleep(millis))

  /** VIRTUAL TIME from `start`: a sleep moves it on at once — hours of retries and timeouts in no time, the same each
   * run. The time it ends at, and the value */
  def virtual[R[_], Rest[_], A](start: Long)(p: A ! R)(using Split[Clock, R, Rest]): (Long, A) ! Rest =
    handle(p, Handler.stateful[Clock, Rest, A, (Long, A), Long](start)((t, a) => pure((t, a)))(
      [X] => (op: Clock[X], cell: Cell[Long]) => op match
        case Now() => cell.get
        case Sleep(millis) => cell.set(cell.get + millis)))

  /** the system's time, a sleep waiting for real (the JVM and Scala Native: Scala.js has no blocking sleep) */
  def system[R[_], Rest[_], A](p: A ! R)(using Split[Clock, R, Rest]): A ! Rest =
    handle(p, Handler.tail[Clock, Rest, A, A](pure)([X] => (op: Clock[X]) => op match
      case Now() => System.currentTimeMillis()
      case Sleep(millis) => Thread.sleep(millis)))

/** a log's levels */
enum Level:
  case Debug, Info, Warn, Error

/** a log: entries of a level and a message */
enum Log[X]:
  case Entry(level: Level, message: String) extends Log[Unit]

object Log:
  def log[R[_]](level: Level, message: String)(using In[Log, R]): Unit ! R = perform[Log](Entry(level, message))
  def debug[R[_]](message: String)(using In[Log, R]): Unit ! R = log(Level.Debug, message)
  def info[R[_]](message: String)(using In[Log, R]): Unit ! R = log(Level.Info, message)
  def warn[R[_]](message: String)(using In[Log, R]): Unit ! R = log(Level.Warn, message)
  def error[R[_]](message: String)(using In[Log, R]): Unit ! R = log(Level.Error, message)

  /** the entries of `least` and above, in order, and the value */
  def collect[R[_], Rest[_], A](least: Level = Level.Debug)(p: A ! R)(using Split[Log, R, Rest])
    : (List[(Level, String)], A) ! Rest =
    handle(p, Handler.stateful[Log, Rest, A, (List[(Level, String)], A), List[(Level, String)]](Nil)(
      (entries, a) => pure((entries.reverse, a)))(
      [X] => (op: Log[X], cell: Cell[List[(Level, String)]]) => op match
        case Entry(level, message) => if level.ordinal >= least.ordinal then cell.set((level, message) :: cell.get)))

  /** each entry of `least` and above to `sink`, as it is logged: the console, a file, a logging library */
  def to[R[_], Rest[_], A](sink: (Level, String) => Unit, least: Level = Level.Debug)(p: A ! R)(
    using Split[Log, R, Rest]): A ! Rest =
    handle(p, Handler.tail[Log, Rest, A, A](pure)([X] => (op: Log[X]) => op match
      case Entry(level, message) => if level.ordinal >= least.ordinal then sink(level, message)))
