package icfpc2013

import scala.util.Random
import scala.collection.mutable.ListBuffer

object ProgramGenerator {
  private[this] val randIds: ListBuffer[Int] = Random.shuffle(1 to 100000).to[ListBuffer]

  private[this] def getOp0Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id]): Stream[Expression] =
    boundVariables.toStream #::: One #:: Zero #:: getExpressions(size - 1, operators, boundVariables)

  private[this] def getOp1Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id]): Stream[Expression] =
    for {
      operator <- operators.collect({ case x: Operator1 => x }).toStream
      expression <- getExpressions(size - 1, operators, boundVariables)
    } yield Op1(operator, expression)

  private[this] def getOp2Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id]): Stream[Expression] =
    for {
      operator <- operators.collect({ case x: Operator2 => x }).toStream
      size1 <- 1 to (size - 2)
      size2 = size - size1 - 1
      expression1 <- getExpressions(size1, operators, boundVariables)
      expression2 <- getExpressions(size2, operators, boundVariables)
    } yield Op2(operator, expression1, expression2)

  private[this] def getIfExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id]): Stream[Expression] =
    if(!operators.contains(If0)) Stream.empty
    else for {
      size1 <- (1 to (size - 3)).toStream
      size2 <- 1 to (size - size1 - 2)
      size3 = size - size1 - size2 - 1
      expression1 <- getExpressions(size1, operators, boundVariables)
      expression2 <- getExpressions(size2, operators, boundVariables)
      expression3 <- getExpressions(size3, operators, boundVariables)
    } yield If(expression1, expression2, expression3)

  private[this] def getFoldExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id]): Stream[Expression] =
    if(!operators.contains(Fold0)) Stream.empty
    else for {
      size1 <- (1 to (size - 4)).toStream
      size2 <- 1 to (size - size1 - 3)
      size3 = size - size1 - size2 - 2
      accId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      xId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      expression1 <- getExpressions(size1, operators - Fold0, boundVariables)
      expression2 <- getExpressions(size2, operators - Fold0, boundVariables)
      expression3 <- getExpressions(size3, operators - Fold0, boundVariables + accId + xId)
    } yield Fold(expression1, expression2, accId, xId, expression3)

  def getExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id]): Stream[Expression] =
    if (size <= 0)
      Stream.empty
    else if (size == 1)
      getOp0Expressions(size, operators, boundVariables)
    else {
      getOp1Expressions(size, operators, boundVariables) #:::
      getOp2Expressions(size, operators, boundVariables) #:::
      getIfExpressions(size, operators, boundVariables) #:::
      getFoldExpressions(size, operators, boundVariables)
    }

  def getPrograms(size: Int,
                  operators: Set[Operator],
                  inputId: Id): Stream[Program] = {
    val exprStream =
      if (operators.contains(Tfold))
        getFoldExpressions(size - 1, operators + Fold0, Set(inputId))
      else getExpressions(size - 1, operators, Set(inputId))

    exprStream.map(Program(inputId, _))
  }
}
