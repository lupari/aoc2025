package assignments

import lib.Input

object Day06:
  val input: List[String] = Input("day06.txt").asList

  def evaluate(expr: List[String]): Long =
    val operands = expr.init.map(_.toLong)
    expr.last match
      case "+" => operands.sum
      case "*" => operands.product

  def partOne(): Long = input.map(_.trim.split(" +")).transpose.map(evaluate).sum

  def partTwo(): Long =
    def groupColumns(cols: List[List[Char]]): List[List[List[Char]]] =
      if cols.isEmpty then Nil
      else
        val (group, rest) = cols.span(_.exists(_.isDigit))
        group :: groupColumns(rest.drop(1))

    def processGroup(group: List[List[Char]]): List[String] =
      val numbers  = group.map(_.init.mkString.trim)
      val operator = group.head.last.toString
      numbers :+ operator

    val cols = input.map(_.toList).transpose
    groupColumns(cols).map(processGroup).map(evaluate).sum
