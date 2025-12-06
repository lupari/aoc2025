package assignments

import scala.annotation.tailrec
import lib.Input

object Day03:
  val banks: List[String] = Input("day03.txt").asList

  def joltage(s: String, k: Int): Long =
    val digits = s.map(_.asDigit)

    def pick(from: Int, remaining: Int): (Long, Int) =
      digits
        .slice(from, digits.length - remaining)
        .zipWithIndex
        .maxBy(_._1) match
        case (digit, ix) => (digit, from + ix)

    @tailrec
    def helper(pos: Int, needed: Int, acc: List[Long]): List[Long] = needed match
      case 0 => acc.reverse
      case x =>
        val (digit, ix) = pick(pos, x - 1)
        helper(ix + 1, x - 1, digit :: acc)

    helper(0, k, Nil).foldLeft(0L)((acc, d) => acc * 10 + d)

  def partOne(): Long = banks.map(joltage(_, k = 2)).sum
  def partTwo(): Long = banks.map(joltage(_, k = 12)).sum
