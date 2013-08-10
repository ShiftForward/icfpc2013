package icfpc2013

import java.io.PrintStream
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import scala.concurrent.duration._
import spray.util._

object PlotSolver extends Solver {

  type State = Unit
  val initialState = ()

  val step = 1L << 32
  val inputs = (1 until 256).scanLeft(0L) { (acc, _) => acc + step }
  val hexInputs = inputs.map { n => "0x" + "%1$16s".format(n.toHexString).replace(' ', '0') }
  val csvScale = 0x100000L

  def solve(problemId: String,
            size: Int,
            ops: Set[Operator],
            inputId: Id = Id("x"),
            knownInputs: Map[Long, Long] = Map(),
            state: State = initialState): (Map[Long, Long], State, Option[Expression]) = {

    val inputsMap =
      if(knownInputs.nonEmpty) knownInputs
      else {
        val response = Client.eval(EvalRequest(Some(problemId), None, hexInputs.toList)).await
        val inputsLong = hexInputs.map(h => HexString.toLong(h.drop(2)))
        val outputsLong = response.outputs.get.map(h => HexString.toLong(h.drop(2)))

        inputsLong.zip(outputsLong).toMap
      }

    val out = new PrintStream("func_chart.csv")
    out.println(inputsMap.map { case (i, o) => i / csvScale + "," + o / csvScale }.mkString("\n"))
    out.close()

    val params = LinearRegression(inputs, inputs.map(inputsMap.apply(_).toDouble)).calc
    println("y = %f + %fx".format(params(0), params(1)))

    val factor = (Math.log(params(1)) / Math.log(2)).toInt

    val expr = if(closeTo(factor, factor.toInt)) {
      if(factor > 0 && closeTo(params(0), 0)) Some(shl(inputId, factor))
      else if(factor < 0 && closeTo(params(0), 0)) Some(shr(inputId, -factor))
      else if(params(1) == 1 && closeTo(params(0), 0)) Some(inputId)
      else if(params(1) == -1 && closeTo(params(0), longHexToDouble("0xFFFFFFFFFFFFFFFF"))) Some(Op1(Not, inputId))
      else None
    } else None

    (knownInputs, (), expr)
  }

  private[this] def closeTo(n1: Double, n2: Double, prec: Double = 0.0001) = Math.abs(n1 - n2) < prec
  private[this] def longHexToDouble(hex: String) = BigInt(hex.drop(2), 16).doubleValue()

  private[this] def shl(expr: Expression, b: Int): Expression =
    if(b == 0) expr
    else shr(Op1(Shl1, expr), b - 1)

  private[this] def shr(expr: Expression, b: Int): Expression =
    if(b == 0) expr
    else if(b >= 16) shr(Op1(Shr16, expr), b - 16)
    else if(b >= 4) shr(Op1(Shr4, expr), b - 4)
    else shr(Op1(Shr1, expr), b - 1)
}

case class LinearRegression(inputs: Seq[Long], outputs: Seq[Double]) {

  def calc = {
    val regression = new OLSMultipleLinearRegression()
    val x = inputs.map { n => Array(n.toDouble) }.toArray
    val y = outputs.toArray

    regression.newSampleData(y, x)

    regression.estimateRegressionParameters()
  }
}
