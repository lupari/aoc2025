package assignments

import scala.annotation.tailrec
import scala.io.Source
import lib.GridExtensions.*
import lib.Points.Point

object Day04:
  val grid: Grid[Char] = Source.fromResource("day04.txt").mkString.toGrid

  def isRemovable(p: Point, g: Grid[Char]): Boolean =
    p.surroundings.flatMap(g.get).count(_ == '@') < 4
  def removableRolls(g: Grid[Char]): Set[Point] =
    g.filter((k, v) => v == '@' && isRemovable(k, g)).keys.toSet

  @tailrec
  def removeAll(g: Grid[Char], removed: Int = 0): Int = removableRolls(g) match
    case ps if ps.isEmpty => removed
    case ps =>
      val grid2 = ps.foldLeft(g)((acc, p) => acc.updated(p, '.'))
      removeAll(grid2, removed + ps.size)

  def partOne(): Int = removableRolls(grid).size
  def partTwo(): Int = removeAll(grid)
