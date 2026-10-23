package okay.scala3

/** A CHOICE among values: its handlers resume the rest once for each — MULTI-SHOT — the alternatives not yet tried
 * kept as data, so a `cut` can drop them */
enum Choose[X]:
  case Among[X](xs: List[X]) extends Choose[X]
  /** the alternatives pending now: where a `cut` goes back to */
  case Mark() extends Choose[Int]
  /** the alternatives made since the mark dropped: the ways not yet tried there are not tried */
  case Cut(mark: Int) extends Choose[Unit]

object Choose:
  def among[X, R[_]](xs: List[X])(using In[Choose, R]): X ! R = perform[Choose](Among(xs))
  def flip[R[_]](using In[Choose, R]): Boolean ! R = among(List(true, false))
  /** no value: this way gives nothing */
  def none[X, R[_]](using In[Choose, R]): X ! R = among(Nil)
  /** a way going on only where `cond` holds */
  def guard[R[_]](cond: Boolean)(using In[Choose, R]): Unit ! R = if cond then unit else none
  def mark[R[_]](using In[Choose, R]): Int ! R = perform[Choose](Mark())
  def cut[R[_]](mark: Int)(using In[Choose, R]): Unit ! R = perform[Choose](Cut(mark))
  /** `p`'s first way, its other ways dropped once it has one — Prolog's `once`: a failure after it does not come back
   * into `p`'s choices */
  def once[A, R[_]](p: => A ! R)(using In[Choose, R]): A ! R =
    mark[R].flatMap(m => p.flatMap(a => cut[R](m).as(a)))

  /** THE ALTERNATIVES NOT YET TRIED, as data: each a way on, given the alternatives below it; `depth` their number,
   * the mark a cut goes back to. A choice goes on with its first value at once and pushes its others here, so a cut
   * drops them and they are gone */
  final class Pending[F](val stack: List[Pending[F] => F], val depth: Int):
    def push(alternative: Pending[F] => F): Pending[F] = Pending(alternative :: stack, depth + 1)
    def cutTo(mark: Int): Pending[F] = if mark >= depth then this else Pending(stack.drop(depth - mark), mark)
    /** the next alternative, given those below it; or `none` when there is none */
    def back(none: => F): F = stack match
      case Nil => none
      case alternative :: below => alternative(Pending(below, depth - 1))

  object Pending:
    def empty[F]: Pending[F] = Pending(Nil, 0)

  /** every value of every way the choices can go, in order */
  def all[R[_], Rest[_], A](p: A ! R)(using Split[Choose, R, Rest]): List[A] ! Rest =
    // an alternative's value: given the values found so far, reversed, the program giving them all
    type F = List[A] => List[A] ! Rest
    type Go = Pending[F] => F
    val done: F = found => okay.scala3.pure(found.reverse)
    def ways[X](xs: List[X], k: X => Go ! Rest, pending: Pending[F]): F = xs match
      case Nil => pending.back(done)
      case x :: Nil => found => k(x).flatMap(go => go(pending)(found))
      case x :: more => found => k(x).flatMap(go => go(pending.push(below => ways(more, k, below)))(found))
    handle(p, Handler.general[Choose, Rest, A, Go](a => okay.scala3.pure(pending => found => pending.back(done)(a :: found)))(
      [X] => (op: Choose[X], k: X => Go ! Rest) => op match
        case Among(xs) => okay.scala3.pure[Go, Rest](pending => ways(xs, k, pending))
        case Mark() => okay.scala3.pure[Go, Rest](pending => found => k(pending.depth).flatMap(go => go(pending)(found)))
        case Cut(m) => okay.scala3.pure[Go, Rest](pending => found => k(()).flatMap(go => go(pending.cutTo(m))(found)))))
      .flatMap(go => go(Pending.empty)(Nil))

  /** the first way the choices go to a value, the others not tried; none, `None` */
  def first[R[_], Rest[_], A](p: A ! R)(using Split[Choose, R, Rest]): Option[A] ! Rest =
    type F = Option[A] ! Rest
    type Go = Pending[F] => F
    val done: F = okay.scala3.pure(None)
    def ways[X](xs: List[X], k: X => Go ! Rest, pending: Pending[F]): F = xs match
      case Nil => pending.back(done)
      case x :: Nil => k(x).flatMap(go => go(pending))
      case x :: more => k(x).flatMap(go => go(pending.push(below => ways(more, k, below))))
    handle(p, Handler.general[Choose, Rest, A, Go](a => okay.scala3.pure(_ => okay.scala3.pure(Some(a))))(
      [X] => (op: Choose[X], k: X => Go ! Rest) => op match
        case Among(xs) => okay.scala3.pure[Go, Rest](pending => ways(xs, k, pending))
        case Mark() => okay.scala3.pure[Go, Rest](pending => k(pending.depth).flatMap(go => go(pending)))
        case Cut(m) => okay.scala3.pure[Go, Rest](pending => k(()).flatMap(go => go(pending.cutTo(m))))))
      .flatMap(go => go(Pending.empty))
