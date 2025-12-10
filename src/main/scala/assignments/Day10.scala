package assignments

import com.google.ortools.Loader
import com.google.ortools.linearsolver.MPSolver
import lib.{Graphs, Input}

object Day10:
  case class Machine(target: List[Boolean], buttons: List[List[Int]], joltage: List[Int])
  def parse(s: String): Machine = s match
    case s"[$a] $b {$c}" =>
      Machine(
        a.map(_ == '#').toList,
        b.split(" ").map(_.tail.init.split(",").map(_.toInt).toList).toList,
        c.split(",").map(_.toInt).toList
      )

  val machines = Input("day10.txt").asList.map(parse)

  def partOne(): Int = machines.map { m =>
    val toggle = (state: List[Boolean], btn: List[Int]) =>
      val idxs = btn.filter(_ < state.length).toSet
      state.zipWithIndex.map { case (v, i) => if idxs.contains(i) then !v else v }
    Graphs.bfs.search(List.fill(m.target.length)(false))(state => m.buttons.map(toggle(state, _)))(
      _ == m.target
    ).map(_._1).get
  }.sum

  def partTwo(): Int =
    Loader.loadNativeLibraries()
    def presses(machine: Machine): Int =
      val solver    = MPSolver.createSolver("SCIP")
      val objective = solver.objective()
      val variables =
        machine.buttons.indices.map(i => solver.makeIntVar(0.0, Double.PositiveInfinity, s"x$i"))
      variables.foreach(objective.setCoefficient(_, 1.0))
      objective.setMinimization()
      machine.joltage.zipWithIndex.foreach { case (j, i) =>
        val c = solver.makeConstraint(j.toDouble, j.toDouble, s"jolts$i")
        machine.buttons.zipWithIndex.filter(_._1.contains(i)).foreach {
          case (_, ix) => c.setCoefficient(variables(ix), 1.0)
        }
      }
      solver.solve()
      variables.map(_.solutionValue().toInt).sum

    machines.map(presses).sum
