package icfpc2013

import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.math._
import icfpc2013.Operator._

object ProgramGenerator {
  private[this] val randIds: ListBuffer[Int] = Random.shuffle(1 to 100000).to[ListBuffer]

  private[this] def getOp0Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    if (requiredOperators > 0)
      Stream.empty
    else
      boundVariables.toStream #::: One #:: Zero #:: getExpressions(size - 1, operators, boundVariables, requiredOperators)

  private[this] def getOp1Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    for {
      operator <- operators.collect({ case x: Operator1 => x }).toStream
      expression <- getExpressions(size - 1, operators, boundVariables, requiredOperators & ~(1 << operator.id))
    } yield Op1(operator, expression)

  private[this] def getOp2Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    for {
      operator <- operators.collect({ case x: Operator2 => x }).toStream
      size1 <- ceil((size - 1) / 2.0).toInt to (size - 2)
      size2 = size - size1 - 1
      expression1 <- getExpressions(size1, operators, boundVariables, 0)
      expression2 <- {
        def s = getExpressions(size2, operators, boundVariables, requiredOperators & ~(1 << operator.id | expression1.operatorIds))
        if (operator != And || expression1.staticValue != Some(0L) || s.isEmpty)
          s
        else
          Zero #:: Stream.empty // dummy stream
      }
    } yield {
      if (operator == And && (expression1.staticValue == Some(0L) || expression2.staticValue == Some(0L)))
        Zero
      else if (operator == Or && expression1.staticValue == Some(0L))
        expression2
      else if (operator == Or && expression2.staticValue == Some(0L))
        expression1
      else
        Op2(operator, expression1, expression2)
    }

  private[this] def getIfExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    if (!operators.contains(If0)) Stream.empty
    else for {
      size1 <- (1 to (size - 3)).toStream
      size2 <- 1 to (size - size1 - 2)
      size3 = size - size1 - size2 - 1
      expression1 <- getExpressions(size1, operators, boundVariables, 0)
      expression2 <- getExpressions(size2, operators, boundVariables, 0)
      expression3 <- {
        def s = getExpressions(size3, operators, boundVariables, requiredOperators & ~(1 << If0.id | expression1.operatorIds | expression2.operatorIds))
        if (expression1.staticValue != Some(0L) || s.isEmpty)
          s
        else
          Zero #:: Stream.empty // dummy stream
      }
    } yield {
      if (expression1.staticValue == Some(0L))
        expression2
      else
        If(expression1, expression2, expression3)
    }

  private[this] def getFoldExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    if (!operators.contains(Fold0)) Stream.empty
    else for {
      size1 <- (1 to (size - 4)).toStream
      size2 <- 1 to (size - size1 - 3)
      size3 = size - size1 - size2 - 2
      xId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      accId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      expression1 <- getExpressions(size1, operators - Fold0, boundVariables, 0)
      expression2 <- getExpressions(size2, operators - Fold0, boundVariables, 0)
      expression3 <- getExpressions(size3, operators - Fold0, boundVariables + accId + xId, requiredOperators & ~(1 << Fold0.id | expression1.operatorIds | expression2.operatorIds))
    } yield Fold(expression1, expression2, xId, accId, expression3)

  private[this] def getTFoldExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    if (!operators.contains(Tfold)) Stream.empty
    else {
      val xId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      val accId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      for {
        expression <- getExpressions(size - 4, operators - Tfold, boundVariables + xId + accId, requiredOperators & ~(1 << Tfold.id))
      } yield Fold(Id("x"), Zero, xId, accId, expression)
    }

  private[this] def getBonusExpressions(
    size: Int,
    operators: Set[Operator],
    inputId: Id,
    requiredOperators: Int): Stream[Expression] =
    for {
      size1 <- (4 to (size - 11)).toStream // If, And, One, 4 for expression2 and 4 for expression3
      size2 <- 4 to (size - size1 - 7)
      size3 = size - size1 - size2 - 3
      expression1 <- getExpressions(size1, operators, Set(inputId), 0)
      expression2 <- getExpressions(size2, operators, Set(inputId), 0)
      expression3 <- getExpressions(size3, operators, Set(inputId), requiredOperators & ~(1 << If0.id | expression1.operatorIds | expression2.operatorIds))
    } yield If(Op2(And, expression1, One), expression2, expression3)

  private[this] def getExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Stream[Expression] =
    if (size <= 0 || requiredOperators.map(_.staticSize).sum >= size)
      Stream.empty
    else if (size == 1)
      getOp0Expressions(size, operators, boundVariables, requiredOperators)
    else {
      getOp1Expressions(size, operators, boundVariables, requiredOperators) #:::
      getOp2Expressions(size, operators, boundVariables, requiredOperators) #:::
      getIfExpressions(size, operators, boundVariables, requiredOperators) #:::
      getFoldExpressions(size, operators, boundVariables, requiredOperators)
    }

  def getPrograms(
    size: Int,
    operators: Set[Operator],
    inputId: Id,
    useAllOperators: Boolean = false): Stream[Program] = {
    def exprStream =
      if (operators.contains(Tfold))
        getTFoldExpressions(size - 1, operators, Set(inputId), if (useAllOperators) operators else 0)
      else if(operators.contains(Bonus))
        getBonusExpressions(size - 1, operators - Bonus, inputId, if (useAllOperators) operators else 0)
      else getExpressions(size - 1, operators, Set(inputId), if (useAllOperators) operators else 0)

    exprStream.map(Program(inputId, _))
  }
}

object TestGenerator extends App {
  BruteForceSolver.genSolveAndGuess(TrainRequest(Some(args(0).toInt), None))
}
