package assignments

import lib.GridExtensions.Grid
import lib.Input
import lib.Points.Point

object Day07:
  val grid: Grid[Char] = Input("day07.txt").asGrid
  val start: Point     = grid.find(_._2 == 'S').get._1
  val maxY: Int        = grid.keys.maxBy(_.y).y
  def split(p: Point): Set[Point] =
    if grid(p) == '^' then Set(p.left(), p.right()) else Set(p)

  def partOne(): Int =
    def step(state: (Set[Point], Set[Point])) =
      val (beams, splits) = state
      val (beams2, splits2) =
        (for
          b <- beams
          next     = b.below()
          advanced = split(next)
        yield (advanced, Option.when(advanced.size > 1)(next))).unzip
      (beams ++ beams2.flatten, splits ++ splits2.flatten)

    Iterator.iterate((Set(start), Set.empty[Point]))(step).drop(maxY).next()._2.size

  def partTwo(): Long =
    def step(state: (Int, Map[Int, Long])) =
      val (y, visits) = state
      val visits2 =
        (for
          (x, count) <- visits.iterator
          advanced   <- split(Point(x, y).below())
        yield advanced.x -> count).toSeq.groupMapReduce(_._1)(_._2)(_ + _)
      (y + 1, visits2)

    Iterator.iterate((start.y, Map(start.x -> 1L)))(step).drop(maxY).next()._2.values.sum
