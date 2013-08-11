package icfpc2013

import scala.util.Random
import scala.collection.mutable.{HashSet, ListBuffer, HashMap => MutableHashMap}
import scala.math._
import icfpc2013.Operator._

object ProgramGenerator {
  private[this] val randIds: ListBuffer[Int] = Random.shuffle(1 to 100000).to[ListBuffer]
  private[this] var cache = MutableHashMap[(Int, Int), Set[Expression]]()

  private[this] def getOp0Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] =
    if (requiredOperators > 0)
      Iterator.empty
    else
      boundVariables.toIterator ++ Iterator(One) ++ Iterator(Zero) ++ getExpressions(size - 1, operators, boundVariables, requiredOperators)

  private[this] def getOp1Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] = {

    val visited = HashSet[Long]()
    for {
      operator <- operators.collect({ case x: Operator1 => x }).toIterator
      expression <- getExpressions(size - 1, operators, boundVariables, requiredOperators & ~(1 << operator.id))
      expressionToYield = {
        if ((operator == Shl1 || operator == Shr1 || operator == Shr4 || operator == Shr16) && expression.staticValue == Some(0L))
          expression
        else
          Op1(operator, expression)
      }
      if expressionToYield.staticValue.isEmpty || !visited.contains(expressionToYield.staticValue.get)
    } yield {
      expressionToYield.staticValue.map(visited +=)
      expressionToYield
    }
  }

  private[this] def getOp2Expressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] = {
    val visited = HashSet[Long]()
    for {
      operator <- operators.collect({ case x: Operator2 => x }).toIterator
      size1 <- ceil((size - 1) / 2.0).toInt to (size - 2)
      size2 = size - size1 - 1
      expression1 <- getExpressions(size1, operators, boundVariables, 0)
      expression2 <- {
        def s = getExpressions(size2, operators, boundVariables, requiredOperators & ~(1 << operator.id | expression1.operatorIds))
        if (operator != And || expression1.staticValue != Some(0L) || s.isEmpty)
          s
        else
          Iterator(Zero) ++ Stream.empty // dummy stream
      }
      expressionToYield = {
        if (operator == And && (expression1.staticValue == Some(0L) || expression2.staticValue == Some(0L)))
          Zero
        else if (operator == And && expression1.staticValue == Some(-1L))
          expression2
        else if (operator == And && expression2.staticValue == Some(-1L))
          expression1
        else if (operator == Or && expression1.staticValue == Some(-1L))
          expression1
        else if (operator == Or && expression2.staticValue == Some(-1L))
          expression2
        else if ((operator == Or || operator == Xor || operator == Plus) && expression1.staticValue == Some(0L))
          expression2
        else if ((operator == Or || operator == Xor || operator == Plus) && expression2.staticValue == Some(0L))
          expression1
        else if ((operator == And || operator == Or) && (expression1 == expression2 || expression1.isStaticallyEqualTo(expression2) ))
          expression1
        else if ((operator == Xor) && (expression1 == expression2 || expression1.isStaticallyEqualTo(expression2) ))
          Zero
        else
          Op2(operator, expression1, expression2)
      }
      if expressionToYield.staticValue.isEmpty || !visited.contains(expressionToYield.staticValue.get)
    } yield {
      expressionToYield.staticValue.foreach(visited +=)
      expressionToYield
    }
  }

  private[this] def getIfExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] = {
    val visited = HashSet[Long]()
    if (!operators.contains(If0)) Iterator.empty
    else for {
      size1 <- (1 to (size - 3)).toIterator
      size2 <- 1 to (size - size1 - 2)
      size3 = size - size1 - size2 - 1
      expression1 <- getExpressions(size1, operators, boundVariables, 0)
      expression2 <- getExpressions(size2, operators, boundVariables, 0)
      expression3 <- {
        def s = getExpressions(size3, operators, boundVariables, requiredOperators & ~(1 << If0.id | expression1.operatorIds | expression2.operatorIds))
        if (expression1.staticValue != Some(0L) || s.isEmpty)
          s
        else
          Iterator(Zero) ++ Iterator.empty // dummy stream
      }
      expressionToYield =
        if (expression1.staticValue == Some(0L))
          expression2
        else if (expression2 == expression3 || expression2.isStaticallyEqualTo(expression3))
          expression2
        else
          If(expression1, expression2, expression3)

      if expressionToYield.staticValue.isEmpty || !visited.contains(expressionToYield.staticValue.get)
    } yield {
      expressionToYield.staticValue.foreach(visited +=)
      expressionToYield
    }
  }

  private[this] def getFoldExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] = {
    val visited = HashSet[Long]()
    val xId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
    val accId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
    if (!operators.contains(Fold0)) Iterator.empty
    else for {
      size1 <- (1 to (size - 4)).toIterator
      size2 <- 1 to (size - size1 - 3)
      size3 = size - size1 - size2 - 2
      expression1 <- getExpressions(size1, operators - Fold0, boundVariables, 0)
      expression2 <- getExpressions(size2, operators - Fold0, boundVariables, 0)
      expression3 <- getExpressions(size3, operators - Fold0, boundVariables + accId + xId, requiredOperators & ~(1 << Fold0.id | expression1.operatorIds | expression2.operatorIds))
      expressionToYield = Fold(expression1, expression2, xId, accId, expression3)
      if expressionToYield.staticValue.isEmpty || !visited.contains(expressionToYield.staticValue.get)
    } yield {
      expressionToYield.staticValue.foreach(visited +=)
      expressionToYield
    }
  }

  private[this] def getTFoldExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] = {
    val visited = HashSet[Long]()
    if (!operators.contains(Tfold)) Iterator.empty
    else {
      val xId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      val accId = Id("x_" + { val v = randIds.head; randIds -= v; randIds += v; v })
      for {
        expression <- getExpressions(size - 4, operators - Tfold, boundVariables + xId + accId, requiredOperators & ~(1 << Tfold.id))
        expressionToYield = Fold(Id("x"), Zero, xId, accId, expression)
        if expressionToYield.staticValue.isEmpty || !visited.contains(expressionToYield.staticValue.get)
      } yield {
        expressionToYield.staticValue.foreach(visited +=)
        expressionToYield
      }
    }
  }

  private[this] def getBonusExpressions(
    size: Int,
    operators: Set[Operator],
    inputId: Id,
    requiredOperators: Int): Iterator[Expression] = {
    val visited = HashSet[Long]()
    for {
      size1 <- (4 to (size - 11)).toIterator // If, And, One, 4 for expression2 and 4 for expression3
      size2 <- 4 to (size - size1 - 7)
      size3 = size - size1 - size2 - 3
      expression1 <- getExpressions(size1, operators, Set(inputId), 0)
      expression2 <- getExpressions(size2, operators, Set(inputId), 0)
      expression3 <- getExpressions(size3, operators, Set(inputId), requiredOperators & ~(1 << If0.id | expression1.operatorIds | expression2.operatorIds))
      expressionToYield = If(Op2(And, expression1, One), expression2, expression3)
      if expressionToYield.staticValue.isEmpty || !visited.contains(expressionToYield.staticValue.get)
    } yield {
      expressionToYield.staticValue.foreach(visited +=)
      expressionToYield
    }
  }

  private[this] def getExpressions(
    size: Int,
    operators: Set[Operator],
    boundVariables: Set[Id],
    requiredOperators: Int): Iterator[Expression] =
    if (size <= 0 || sumStaticSize(requiredOperators.toSeq) >= size)
      Iterator.empty
    else if (size == 1)
      getOp0Expressions(size, operators, boundVariables, requiredOperators)
    else {
      if (cache.contains((size, requiredOperators)))
        cache((size, requiredOperators)).toIterator
      else {
        val exps = getOp1Expressions(size, operators, boundVariables, requiredOperators) ++
                   getOp2Expressions(size, operators, boundVariables, requiredOperators) ++
                   getIfExpressions(size, operators, boundVariables, requiredOperators) ++
                   getFoldExpressions(size, operators, boundVariables, requiredOperators)

        if (size < 11) {
          val result = exps.toSet
          cache += ((size, requiredOperators) -> result)
          result.toIterator
        } else exps
      }
    }

  @inline private[this] def sumStaticSize(s: Seq[Operator]): Int = {
    var i = 0
    var res = 0
    while (i < s.size) {
      res += s(i).staticSize
      i += 1
    }
    res
  }

  def getPrograms(
    size: Int,
    operators: Set[Operator],
    inputId: Id,
    useAllOperators: Boolean = false): Stream[Program] = {

    cache = MutableHashMap[(Int, Int), Set[Expression]]()

    def exprStream =
      if (operators.contains(Tfold))
        getTFoldExpressions(size - 1, operators, Set(inputId), if (useAllOperators) operators else 0)
      else if(operators.contains(Bonus))
        getBonusExpressions(size - 1, operators - Bonus, inputId, if (useAllOperators) operators else 0)
      else getExpressions(size - 1, operators, Set(inputId), if (useAllOperators) operators else 0)

    exprStream.toStream.map(Program(inputId, _))
  }
}

object TestGenerator extends App {
  BruteForceSolver.genSolveAndGuess(TrainRequest(Some(args(0).toInt), None))
}
