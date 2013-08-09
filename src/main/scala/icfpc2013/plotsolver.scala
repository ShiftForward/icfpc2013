package icfpc2013

import java.io.PrintStream
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import scala.concurrent.duration._
import spray.util._

object PlotSolver {

  val step = 1L << 32
  val inputs = (1 until 256).scanLeft(0L) { (acc, _) => acc + step }
  val hexInputs = inputs.map { n => "0x" + "%1$16s".format(n.toHexString).replace(' ', '0') }

  def closeTo(n1: Double, n2: Double, prec: Double = 0.0001) = Math.abs(n1 - n2) < prec
  def longHexToDouble(hex: String) = BigInt(hex.drop(2), 16).doubleValue()

  def solve(problemId: String): Double = {
    implicit val timeout = 10 seconds
    val response = Client.eval(EvalRequest(Some(problemId), None, hexInputs.toList)).await

    val hexOutputs = response.outputs.get
    val outputs = hexOutputs.map(longHexToDouble)

    val pairs = inputs.zip(outputs)

    val out = new PrintStream("func_chart.csv")
    out.println(pairs.map { case (i, o) => i / 0x100000 + "," + o / 0x100000 }.mkString("\n"))
    out.close()

    val params = LinearRegression(inputs, outputs).calc
    println("y = %f + %fx".format(params(0), params(1)))

    val factor = (Math.log(params(1)) / Math.log(2)).toInt

    if(closeTo(factor, factor.toInt)) {
      if(factor > 0) println("Operator is shl" + factor)
      else if(factor < 0) println("Operator is shr" + -factor)
      else if(params(1) == 1) println("Operator is identity")
      else if(params(1) == -1 && closeTo(params(0), longHexToDouble("0xFFFFFFFFFFFFFFFF"))) println("Operator is not")

    } else {
      println("Operator is other thing")
    }

    params(1)
  }
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
