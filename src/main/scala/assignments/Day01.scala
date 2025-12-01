package assignments

import scala.io.Source

object Day01:

  def parse(l: String): Int = l.head match
    case 'L' => -l.tail.toInt
    case 'R' => l.tail.toInt

  val instructions: List[Int] = Source.fromResource("day01.txt").getLines.map(parse).toList

  def turn(pos: Int, instruction: Int): Int = Math.floorMod(pos + instruction, 100)

  def partOne(): Int = instructions.scanLeft(50)(turn).count(_ == 0)
  def partTwo(): Int = instructions.flatMap(i => Seq.fill(i.abs)(i.sign)).scanLeft(50)(turn).count(_ == 0)
