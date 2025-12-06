package assignments

import scala.collection.immutable.NumericRange
import lib.Input

object Day02:

  val input: String = Input("day02.txt").asString
  val ranges: Seq[NumericRange.Inclusive[Long]] =
    input.split(",").toSeq.map { case s"$s-$e" => s.toLong to e.toLong }

  def isInvalid(s: String): Boolean = s.take(s.length / 2) == s.drop(s.length / 2)
  def isInvalid2(s: String): Boolean =
    (1 to s.length / 2).exists(k => s.grouped(k).distinct.size == 1)

  def partOne(): Long = ranges.flatMap(_.filter(n => isInvalid(n.toString))).sum
  def partTwo(): Long = ranges.flatMap(_.filter(n => isInvalid2(n.toString))).sum
