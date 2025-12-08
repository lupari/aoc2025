package assignments

import lib.Input
import lib.Points.Point3
import lib.Search.UnionFind

object Day08:
  def parse(line: String): Point3 = line match
    case s"$a,$b,$c" => Point3(a.toInt, b.toInt, c.toInt)
  val boxes: List[Point3] = Input("day08.txt").asList.map(parse)
  val pairDistances: List[(Point3, Point3)] =
    boxes.combinations(2).collect { case List(p1, p2) => (p1, p2) }
      .toList.sortBy { case (p1, p2) => p1.distance(p2) }

  case class CircuitState(components: Int, closure: Option[(Point3, Point3)])
  def circuitStates: Iterator[(CircuitState, UnionFind[Point3])] =
    val (uf, state0) = (UnionFind(boxes.toSet), CircuitState(boxes.size, None))
    pairDistances.iterator.scanLeft((state0, uf)) { case ((state, uf), pair) =>
      if uf.union(pair) then // joined two components
        val components2 = state.components - 1
        val connection2 =
          if components2 == 1 && state.closure.isEmpty then Some(pair) else state.closure
        (state.copy(components = components2, closure = connection2), uf)
      else (state, uf)
    }

  def partOne(): Int =
    val (_, uf) = circuitStates.drop(1000).next()
    boxes.toSet.groupBy(uf.find).values.toList.map(_.size).sorted.takeRight(3).product

  def partTwo(): Int =
    circuitStates.flatMap(_._1.closure).next() match { case (a, b) => a.x * b.x }
