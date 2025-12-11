package lib

import scala.annotation.tailrec

/** Brent's cycle detection algorithm for finding cycles in sequences.
  *
  * Example usage:
  * {{{
  *   // Find cycle in a function: f(x) = (x^2 + 1) mod 255
  *   val f = (x: Int) => (x * x + 1) % 255
  *   val x0 = 3
  *
  *   // Get cycle length and start index
  *   val (lambda, mu) = Cycles.brent(f, x0)
  *   // lambda = cycle length, mu = index where cycle starts
  *
  *   // Get the actual cycle sequence
  *   val cycleSeq = Cycles.cycle(f, x0)
  *   // Returns the repeating cycle values
  * }}}
  */
object Cycles:
  /** Returns (lambda, mu) where:
    *   - lambda: length of the cycle
    *   - mu: index where the cycle starts
    */
  def brent[A](f: A => A, x0: A): (Int, Int) =
    val lambda = findLambda(f, x0)
    val mu     = findMu(f, x0, lambda)
    (lambda, mu)

  /** Returns the cycle sequence starting from the cycle entry point. */
  def cycle[A](f: A => A, x0: A): Seq[A] =
    val (lambda, mu) = brent(f, x0)
    // Build the sequence more efficiently by iterating from x0
    Iterator
      .iterate(x0)(f)
      .drop(mu)
      .take(lambda)
      .toSeq

  private def findLambda[A](f: A => A, x0: A): Int =
    findLambdaRec(f, tortoise = x0, hare = f(x0), power = 1, lambda = 1)

  private def findMu[A](f: A => A, x0: A, lambda: Int): Int =
    val hare = Iterator.iterate(x0)(f).drop(lambda).next()
    findMuRec(f, tortoise = x0, hare, mu = 0)

  @tailrec
  private def findLambdaRec[A](
      f: A => A,
      tortoise: A,
      hare: A,
      power: Int,
      lambda: Int
  ): Int =
    if tortoise == hare then lambda
    else
      val (newTortoise, newPower, newLambda) =
        if power == lambda then (hare, power * 2, 0)
        else (tortoise, power, lambda)
      findLambdaRec(f, newTortoise, f(hare), newPower, newLambda + 1)

  @tailrec
  private def findMuRec[A](f: A => A, tortoise: A, hare: A, mu: Int): Int =
    if tortoise == hare then mu
    else findMuRec(f, f(tortoise), f(hare), mu + 1)
