package assignments

import lib.{Input, Graphs}

object Day11:
  def parse(line: String): (String, List[String]) = line match
    case s"$a: $b" => (a, b.split(" ").toList)
  val devices: Map[String, List[String]] = Input("day11.txt").asList.map(parse).toMap

  def pathCount(start: String, goal: String): Long =
    Graphs.dfs.countPaths(start)(devices.getOrElse(_, Nil))(_ == goal)

  def partOne(): Long = pathCount("you", "out")
  def partTwo(): Long =
    val (f2d, d2f) = (pathCount("fft", "dac"), pathCount("dac", "fft"))
    val (a, b, c)  = if d2f == 0 then ("fft", f2d, "dac") else ("dac", d2f, "fft")
    pathCount("svr", a) * b * pathCount(c, "out")
