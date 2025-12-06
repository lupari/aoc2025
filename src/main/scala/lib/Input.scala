package lib

import lib.GridExtensions.*
import scala.io.Source

case class Input(filename: String):
  private val resource     = Source.fromResource(filename)
  def asString: String     = resource.mkString
  def asList: List[String] = resource.getLines.toList
  def asGrid: Grid[Char]   = asString.toGrid
  def asIntGrid: Grid[Int] = asString.toIntGrid
