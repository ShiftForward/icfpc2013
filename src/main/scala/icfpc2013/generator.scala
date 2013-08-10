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
        if (operator != And || expression1 != Zero)
          getExpressions(size2, operators, boundVariables, requiredOperators & ~(1 << operator.id | expression1.operatorIds))
        else
          Zero #:: Stream.empty
      }
    } yield {
      if (operator == And && (expression1 == Zero || expression2 == Zero))
        Zero
      else if (operator == Or && expression1 == Zero)
        expression2
      else if (operator == Or && expression2 == Zero)
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
      expression3 <- getExpressions(size3, operators, boundVariables, requiredOperators & ~(1 << If0.id | expression1.operatorIds | expression2.operatorIds))
    } yield If(expression1, expression2, expression3)

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
      else getExpressions(size - 1, operators, Set(inputId), if (useAllOperators) operators else 0)

    exprStream.map(Program(inputId, _))
  }
}
