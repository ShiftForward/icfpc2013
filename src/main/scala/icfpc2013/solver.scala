package icfpc2013

import spray.util._

trait Solver {

  def batchSolve(probs: List[Problem], remTime: Int = 2): Boolean = probs match {
    case Nil => println("All problems solved successfully!"); true
    case p :: ps =>
      solveAndGuess(p.id, p.size, p.operators.map(toOperator).toSet) match {
        case None => println("A problem occurred solving problem " + p.id); false
        case Some(guess) =>
          if(guess.status == "win") {
            println("Solved problem " + p.id)
            if(remTime == 1) {
              Thread.sleep(20000)
              batchSolve(ps, 2)
            } else {
              batchSolve(ps, remTime - 1)
            }
          }
          else {
            println("A problem occurred solving problem " + p.id)
            println("Guess response: " + guess)
            false
          }
      }
  }

  def genSolveAndGuess(trainReq: TrainRequest = TrainRequest(Some(3), None)): Option[GuessResponse] = {
    val train = Client.train(trainReq).await
    println("Problem ID is " + train.id + " - " + train.challenge)
    solveAndGuess(train.id, train.size, train.operators.map(toOperator).toSet)
  }

  def solveAndGuess(problemId: String, size: Int, ops: Set[Operator]): Option[GuessResponse] = {
    val inputId = Id("x")

    solve(problemId, size, ops, inputId) match {
      case None =>
        println("Unknown function!")
        None

      case Some(expr) =>
        val prog = Program(inputId, expr)
        Some(Client.guess(Guess(problemId, prog.toString)).await)
    }
  }

  def solve(problemId: String, size: Int, ops: Set[Operator], inputId: Id = Id("x")): Option[Expression]

  // TODO move this to API or to BV entities
  private[this] def toOperator(str: String) = str match {
    case "if0" => If0
    case "fold0" => Fold0
    case "not" => Not
    case "shl1" => Shl1
    case "shr1" => Shr1
    case "shr4" => Shr4
    case "shr16" => Shr16
    case "and" => And
    case "or" => Or
    case "xor" => Xor
    case "plus" => Plus
    case "tfold" => Fold0 // TODO add this to the BV entities
  }
}
