package icfpc2013

import spray.util._

trait Solver {
  type State
  val initialState: State

  def batchSolve(probs: List[Problem]): Boolean = probs match {
    case Nil => println("All problems solved successfully!"); true
    case p :: ps =>
      solveAndGuess(p.id, p.size, p.operators.map(Operator(_)).toSet) match {
        case None => println("A problem occurred solving problem " + p.id); false
        case Some(guess) =>
          if(guess.status == "win") {
            println("Solved problem " + p.id)
            batchSolve(ps)
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
    println("Problem is " + train)
    solveAndGuess(train.id, train.size, train.operators.map(Operator(_)).toSet)
  }

  def solveAndGuess(problemId: String, size: Int, ops: Set[Operator]): Option[GuessResponse] = {
    val inputId = Id("x")

    def solveWithInputs(knownInputs: Map[Long, Long] = Map(),
                        state: State = initialState): Option[GuessResponse] = {

      Client.clearWindowFor(2) // at least for the eval and the guess requests

      solve(problemId, size, ops, inputId, knownInputs, state) match {
        case (_, _, None) =>
          println("Unknown function!")
          None

        case (newKnownInputs, newState, Some(expr)) =>
          val prog = Program(inputId, expr)

          println("Trying program " + prog)

          Client.guess(Guess(problemId, prog.toString)).await match {
            case g @ GuessResponse("win", _, _) => Some(g)

            case GuessResponse("mismatch", Some(in :: out :: wrongOut :: _), _) =>
              println("Mismatch occurred on P(%s): expected %s, found %s. Retrying...".
                format(in, out, wrongOut))

              solveWithInputs(
                Map(HexString.toLong(in.drop(2)) -> HexString.toLong(out.drop(2))),
                newState)
          }
      }
    }
    solveWithInputs()
  }

  def solve(problemId: String,
            size: Int,
            ops: Set[Operator],
            inputId: Id = Id("x"),
            knownInputs: Map[Long, Long] = Map(),
            state: State = initialState): (Map[Long, Long], State, Option[Expression])
}
