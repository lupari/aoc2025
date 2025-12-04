package lib

import lib.Points.Point

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.matching.Regex

object GridExtensions:

  type Grid[A] = Map[Point, A]

  extension [A](grid: Grid[A])
    def keyOf(v: A): Option[Point] = grid.find(_._2 == v).map(_._1)
    def canvas(default: A)(cf: A => A)(using classTag: ClassTag[A]): Array[Array[A]] =
      val (x, y) = (grid.keys.maxBy(_.x).x, grid.keys.maxBy(_.y).y)
      val canvas = Array.tabulate(y + 1, x + 1)((_, _) => default)
      for (point, value) <- grid yield canvas(point.y)(point.x) = cf(value)
      canvas

    def printGrid(default: A, cf: A => A = identity)(using classTag: ClassTag[A]): Unit =
      val canvas = grid.canvas(default)(cf)
      canvas.foreach { row =>
        row.mkString foreach print; println
      }

    def findAll(length: Int, regex: Regex): Set[Point] =
      grid.keySet.flatMap { p =>
        val sequence = (p.x until (p.x + length)).map(x => grid.get(Point(x, p.y)))
        if sequence.forall(_.isDefined) then
          val seq = sequence.flatten.mkString
          Option.when(regex.matches(seq))(p)
        else None
      }
    
    def has(p: Point): Boolean = grid.contains(p) 

  private def makeGrid[A](input: Seq[Char])(fn: (Char => A)): Grid[A] =
    @tailrec
    def helper(xs: Seq[Char], acc: Grid[A], current: Point): Grid[A] =
      xs match
        case h :: t if h == '\n' => helper(t, acc, Point(0, current.y + 1))
        case h :: t => helper(t, acc.updated(current, fn(h)), Point(current.x + 1, current.y))
        case _      => acc
    helper(input, Map.empty, Point(0, 0))

  extension (input: String) def toGrid: Grid[Char]   = makeGrid(input.toList)(identity)
  extension (input: String) def toIntGrid: Grid[Int] = makeGrid(input.toList)(_.asDigit)
