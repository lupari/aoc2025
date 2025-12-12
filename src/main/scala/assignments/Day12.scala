package assignments

import lib.Input

object Day12:
  case class Shape(area: Int)
  case class Region(width: Int, height: Int, counts: List[Int]):
    def fits(shapes: List[Shape]): Boolean =
      (counts lazyZip shapes).map(_ * _.area).sum <= width * height

  def parseShapes(input: List[String]): List[Shape] =
    input
      .dropWhile(!_.matches("\\d+:"))
      .foldLeft(List.empty[Shape]) {
        case (shapes, line) if line.matches("\\d+:") => Shape(0) :: shapes
        case (Shape(area) :: rest, line) if line.nonEmpty =>
          Shape(area + line.count(_ == '#')) :: rest
        case (shapes, _) => shapes
      }
      .reverse

  def parseRegion(line: String): Option[Region] =
    line.trim match
      case s"${w}x${h}: $counts" =>
        Some(Region(w.toInt, h.toInt, counts.trim.split("\\s+").map(_.toInt).toList))

  val input: List[String]   = Input("day12.txt").asList
  val shapes: List[Shape]   = parseShapes(input)
  val regions: List[Region] = input.filter(_.contains("x")).flatMap(parseRegion)

  def partOne(): Int = regions.count(_.fits(shapes))
