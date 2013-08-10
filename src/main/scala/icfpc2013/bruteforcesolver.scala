package icfpc2013

import icfpc2013.BvCompiler._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import spray.util._

object BruteForceSolver extends Solver {
  def getInputs: List[String] = {
    val inputs = ListBuffer[String]()

    inputs += "0x0000000000000000"
    inputs += "0xffffffffffffffff"
    while (inputs.size < 256) {
      inputs += longToHex(Random.nextLong())
    }

    inputs.toList
  }

  def solve(problemId: String, size: Int, ops: Set[Operator], inputId: Id) = {
    val possibleExpressions = ProgramGenerator.getExpressions(size - 1, ops, Set(inputId))
    val inputs = getInputs
    val response = Client.eval(EvalRequest(Some(problemId), None, inputs)).await
    val outputs = response.outputs.get.map(_.toLowerCase)

    val res = possibleExpressions.dropWhile { expression =>
      val program = Program(inputId, expression)
      val f = BvCompiler(program)
      val results = inputs.map(input => f(BvCompiler.hexToLong(input))).map(BvCompiler.longToHex)
      results != outputs
    }

    res.headOption
  }
}
