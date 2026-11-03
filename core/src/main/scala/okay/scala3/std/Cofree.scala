package okay.scala3.std

import okay.scala3.*

/** THE COFREE COMONAD over lists: a value and its branches, each a tree again — what may follow a point (futures that
 * branch). `extract` the value; `extend(f)` f at every subtree: "how risky is everything below this point". Strict:
 * a tree made by a program (`unfold`) is made as far as its budget goes */
final case class Cofree[A](head: A, tail: List[Cofree[A]]):
  def extract: A = head
  def extend[B](f: Cofree[A] => B): Cofree[B] = Cofree(f(this), tail.map(_.extend(f)))
  def duplicate: Cofree[Cofree[A]] = extend(identity)
  def map[B](f: A => B): Cofree[B] = Cofree(f(head), tail.map(_.map(f)))
  /** every value, the head first, depth first */
  def all: List[A] = head :: tail.flatMap(_.all)
  /** the paths from here to the leaves */
  def paths: List[List[A]] = if tail.isEmpty then List(List(head)) else tail.flatMap(_.paths).map(head :: _)
  def size: Int = 1 + tail.map(_.size).sum

object Cofree:
  def leaf[A](a: A): Cofree[A] = Cofree(a, Nil)

  /** a tree unfolded by a program: a seed's branches by `next`, breadth first, at most `budget` nodes made (the seed
   * one of them) — a branch past the budget left a leaf. `next` is asked once a node, in the order made */
  def unfold[A, R[_]](seed: A, budget: Int)(next: A => List[A] ! R): Cofree[A] ! R =
    // the nodes made, by their place: each a value and its children's places
    final case class Made(value: A, children: Vector[Int])
    def go(made: Vector[Made], queue: Vector[Int]): Vector[Made] ! R =
      if queue.isEmpty || made.size >= budget then pure(made)
      else
        val at = queue.head
        next(made(at).value).flatMap { kids =>
          val room = (budget - made.size).max(0)
          val taken = kids.take(room)
          val places = taken.indices.map(made.size + _).toVector
          go((made ++ taken.map(Made(_, Vector.empty))).updated(at, made(at).copy(children = places)),
            queue.tail ++ places)
        }
    go(Vector(Made(seed, Vector.empty)), Vector(0)).map { made =>
      def tree(i: Int): Cofree[A] = Cofree(made(i).value, made(i).children.toList.map(tree))
      tree(0)
    }
