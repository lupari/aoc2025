package assignments

import scala.annotation.tailrec
import scala.io.Source

object Day05:
  case class Range(start: Long, end: Long)
  val input: List[String] = Source.fromResource("day05.txt").getLines().toList
  val fresh: List[Range] =
    input.takeWhile(_.nonEmpty).map { case s"$s-$e" => Range(s.toLong, e.toLong) }

  def partOne(): Long =
    val available = input.dropWhile(_.nonEmpty).tail.map(_.toLong)
    available.count(a => fresh.exists(r => a >= r.start && a <= r.end))

  def partTwo(): Long =
    @tailrec
    def merge(rs: List[Range], acc: List[Range] = Nil): List[Range] =
      rs.sortBy(_.start) match
        case x :: y :: t =>
          if y.start > x.end + 1 then merge(y :: t, x :: acc)
          else merge(Range(x.start, x.end max y.end) :: t, acc)
        case _ => (rs ++ acc).reverse

    merge(fresh).map(r => r.end - r.start + 1).sum
