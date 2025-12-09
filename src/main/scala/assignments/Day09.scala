package assignments

import lib.Input
import lib.Points.*

object Day09:
  def parse(line: String): Point = line match
    case s"$a,$b" => Point(a.toInt, b.toInt)
  def area(p: Point, q: Point): Long = ((p.x - q.x).abs + 1).toLong * ((p.y - q.y).abs + 1).toLong
  val tiles: List[Point]             = Input("day09.txt").asList.map(parse)
  val rectangles: List[(Point, Point, Long)] = tiles.combinations(2)
    .collect { case List(p, q) => (p, q, area(p, q)) }.toList

  def partOne(): Long = rectangles.maxBy(_._3)._3
  def partTwo(): Long =
    val perimeter = tiles.zip(tiles.tail :+ tiles.head).flatMap { case (p, q) =>
      Line(p, q).points
    }.toSet

    // True if a rectangle contains no perimeter points in its interior
    def isValid(p: Point, q: Point): Boolean =
      val box = Box(Point(p.x min q.x, p.y min q.y), Point(p.x max q.x, p.y max q.y))
      !perimeter.exists(box.contains(_, strict = true))

    rectangles.sortBy(-_._3).collectFirst { case (p, q, a) if isValid(p, q) => a }.get
