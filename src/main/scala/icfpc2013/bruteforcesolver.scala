package icfpc2013

import icfpc2013.BvCompiler._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import spray.util._

object BruteForceSolver {
  def getInputs: List[String] = {
    val inputs = ListBuffer[String]()

    inputs += "0x0000000000000000"
    inputs += "0xffffffffffffffff"
    while (inputs.size < 256) {
      inputs += longToHex(Random.nextLong())
    }

    inputs.toList
  }

  def genSolveAndGuess(trainReq: TrainRequest = TrainRequest(Some(4), None)) {
    val train = Client.train(trainReq).await
    println("Problem ID is " + train.id + " - " + train.challenge)
    val operators = Set(If0, Fold0, Not, Shl1, Shr1, Shr4, Shr16, And, Or, Xor, Plus)
    val possibleExpressions = ProgramGenerator.getExpressions(trainReq.size.get - 1, operators, Set(Id("x")))
    val inputs = getInputs
    val outputs = Client.eval(EvalRequest(Some(train.id), None, inputs)).await.outputs.get.map(_.toLowerCase)

    val res = possibleExpressions.dropWhile { expression =>
      val program = Program(Id("x"), expression)
      val f = BvCompiler(program)
      val results = inputs.map(input => f(BvCompiler.hexToLong(input))).map(BvCompiler.longToHex)
      results != outputs
    }

    println("Solution is " + Program(Id("x"), res.head))
  }
}
