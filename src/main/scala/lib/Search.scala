package lib

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.concurrent.TrieMap

object Search:
  object binSearch:
    def apply[A, B](xs: Iterable[A])(decide: Iterable[A] => Option[B]): (Iterable[A], Iterable[A]) =
      @tailrec
      def helper(lower: Int, upper: Int): (Iterable[A], Iterable[A]) =
        if lower >= upper then xs.splitAt(lower)
        else
          val mid = (lower + upper) / 2
          decide(xs.take(mid)) match
            case None => helper(lower, mid)
            case _    => helper(mid + 1, upper)

      helper(0, xs.size)

  class UnionFind[T](elements: Set[T]):
    private val parent: mutable.Map[T, T] = mutable.HashMap.from(elements.map(e => e -> e))
    private val rank: mutable.Map[T, Int] = mutable.HashMap.from(elements.map(e => e -> 0))

    def find(x: T): T =
      if parent(x) != x then parent(x) = find(parent(x))
      parent(x)

    def union(x: T, y: T): Boolean =
      val (rootX, rootY) = (find(x), find(y))
      if rootX == rootY then false
      else
        val (lower, higher) = if rank(rootX) < rank(rootY) then (rootX, rootY) else (rootY, rootX)
        parent(lower) = higher
        if rank(rootX) == rank(rootY) then rank(higher) += 1
        true

    def union(pair: (T, T)): Boolean = union(pair._1, pair._2)

  def memoize[A, B](f: A => B): A => B =
    val cache = TrieMap.empty[A, B]
    arg => cache.getOrElseUpdate(arg, f(arg))