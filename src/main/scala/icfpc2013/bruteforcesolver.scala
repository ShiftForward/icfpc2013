package icfpc2013

import scala.util.Random
import scala.collection.mutable.ListBuffer
import spray.util._

object BruteForceSolver extends Solver {

  type State = Option[Stream[(Expression, Long => Long)]]
  val initialState = None

  def getInputs: List[String] = {
    val inputs = ListBuffer[String]()

    inputs += "0x0000000000000000"
    inputs += "0xFFFFFFFFFFFFFFFF"

    while (inputs.size < 256) {
      inputs += ("0x" + HexString.fromLong(Random.nextLong()))
    }

    inputs.toList
  }

  def solve(problemId: String,
            size: Int,
            ops: Set[Operator],
            inputId: Id = Id("x"),
            knownInputs: Map[Long, Long] = Map(),
            state: State = initialState): (Map[Long, Long], State, Option[Expression]) = {

    val possiblePrograms = state.getOrElse {
      ProgramGenerator.getPrograms(size, ops, inputId, true).map { p =>
        (p.e, BvCompiler(p))
      }
    }

    val inputsMap =
      if(knownInputs.nonEmpty) knownInputs
      else {
        val inputs = getInputs
        val response = Client.eval(EvalRequest(Some(problemId), None, inputs)).await
        val inputsLong = inputs.map(h => HexString.toLong(h.drop(2)))
        val outputsLong = response.outputs.get.map(h => HexString.toLong(h.drop(2)))

        inputsLong.zip(outputsLong).toMap
      }

    val filteredStream = possiblePrograms.filter { program =>
      inputsMap.forall { case (in, out) => program._2(in) == out }
    }

    (inputsMap, Some(filteredStream), filteredStream.headOption.map(_._1))
  }
}
