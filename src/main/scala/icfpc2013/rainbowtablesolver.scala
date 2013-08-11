package icfpc2013

import scala.slick.driver.SQLiteDriver.simple._
import Database.threadLocalSession
import spray.util._

object RainbowTableSolver extends Solver {
  type State = Option[Seq[(Expression, Long => Long)]]
  val initialState = None

  def solve(
    problemId: String,
    size: Int,
    ops: Set[Operator],
    inputId: Id = Id("x"),
    knownInputs: Map[Long, Long] = Map(),
    state: State = initialState): (Map[Long, Long], State, Option[Expression]) = RainbowTable.db withSession {

    require(!(Query(RainbowTables).filter(_.problemId === problemId).list.isEmpty))

    var outputHash = 0

    val inputsMap =
      if (knownInputs.nonEmpty) knownInputs
      else {
        val response = Client.eval(EvalRequest(Some(problemId), None, StaticInput.hex0x)).await
        val outputsLong = response.outputs.get.map(h => HexString.toLong(h.drop(2)))
        outputHash = outputsLong.## // This is very lame, but it's too late...

        StaticInput.long.zip(outputsLong).toMap
      }

    val possiblePrograms = state.getOrElse {
      (for {
        r <- RainbowTables if r.problemId === problemId && r.outputHash === outputHash.toString
      } yield (r.program)).list.map { expression =>
        val program = BvParser(expression).get
        (program.e, BvCompiler(program))
      }
    }

    val filtered = possiblePrograms.filter { program =>
      inputsMap.forall { case (in, out) => program._2(in) == out }
    }

    (inputsMap, Some(filtered), filtered.headOption.map(_._1))
  }
}
