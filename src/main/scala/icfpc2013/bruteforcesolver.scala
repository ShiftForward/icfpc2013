package icfpc2013

import icfpc2013.BvCompiler._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import spray.util._

object BruteForceSolver extends Solver {
  def getInputs: List[String] = {
    val inputs = ListBuffer[String]()

    inputs += "0x0000000000000000"
    inputs += "0xFFFFFFFFFFFFFFFF"

    while (inputs.size < 256) {
      inputs += ("0x" + HexString.fromLong(Random.nextLong()))
    }

    inputs.toList
  }

  def solve(problemId: String, size: Int, ops: Set[Operator], inputId: Id) = {
    val possiblePrograms = ProgramGenerator.getPrograms(size, ops, inputId)
    val inputs = getInputs
    val response = Client.eval(EvalRequest(Some(problemId), None, inputs)).await
    val inputsLong = inputs.map(h => HexString.toLong(h.drop(2)))
    val outputs = response.outputs.get
    val outputsLong = outputs.map(h => HexString.toLong(h.drop(2)))

    val res = possiblePrograms.dropWhile { program =>
      val f = BvCompiler(program)
      val results = inputsLong.map(f(_))
      results != outputsLong
    }

    res.headOption.map(_.e)
  }
}
