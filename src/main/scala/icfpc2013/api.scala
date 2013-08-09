package icfpc2013

import spray.json.DefaultJsonProtocol

case class Problem(
  id: String,
  size: Int,
  operators: List[String],
  solved: Option[Boolean],
  timeLeft: Option[Int])

case class EvalRequest(
  id: Option[String],
  program: Option[String],
  arguments: List[String])

case class EvalResponse(
  status: String,
  outputs: Option[List[String]],
  message: String)

case class Guess(
  id: String,
  program: String)

case class GuessResponse(
  status: String,
  values: Option[List[String]],
  message: Option[String])

case class TrainRequest(
  size: Option[Int],
  operators: Option[String])

case class TrainingProblem(
  challenge: String,
  id: String,
  size: Int,
  operators: List[String])

case class Status(
  easyChairId: String,
  contestScore: Int,
  lightningScore: Int,
  trainingScore: Int,
  mismatches: Int,
  numRequests: Int,
  requestWindow: Window,
  cpuWindow: Window,
  cpuTotalTime: Int)

case class Window(
  resetsIn: Int,
  amount: Int,
  limit: Int)

object JsonApi extends DefaultJsonProtocol {
  implicit val problemFormat = jsonFormat5(Problem)
  implicit val evalRequestFormat = jsonFormat3(EvalRequest)
  implicit val evalResponseFormat = jsonFormat3(EvalResponse)
  implicit val guessFormat = jsonFormat2(Guess)
  implicit val guessResponseFormat = jsonFormat3(GuessResponse)
  implicit val trainRequestFormat = jsonFormat2(TrainRequest)
  implicit val trainingProblemFormat = jsonFormat4(TrainingProblem)
  implicit val windowFormat = jsonFormat3(Window)
  implicit val statusFormat = jsonFormat9(Status)
}
