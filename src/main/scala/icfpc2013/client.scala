package icfpc2013

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.io.IO
import akka.event.Logging
import java.io.PrintStream
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.http.Uri._
import spray.httpx.SprayJsonSupport._

object Client {
  import spray.util._
  import JsonApi._

  val token = "YOUR_TOKEN_HERE"
  val suffix = "vpsH1H"
  val hostname = "icfpc2013.cloudapp.net"

  implicit lazy val system = ActorSystem()
  import system.dispatcher
  val log = Logging(system, getClass)

  implicit val timeout = 10 seconds

  def await[T](f: Future[T]) = Await.result(f, timeout)

  def status: Future[Status] = {
    val pipeline = sendReceive ~> unmarshal[Status]
    val response = pipeline(
      Post(
        Uri.from(
          scheme = "http",
          host = hostname,
          path = "/status",
          query = Query("auth" -> (token + suffix)))))

    response.onComplete {
      case Success(status: Status) =>
      case Success(unexpected) =>
        log.warning("The API call was successful but returned something unexpected: '{}'.", unexpected)
      case Failure(error) =>
        log.error(error, "Something went wrong.")
    }
    response
  }

  def problems: Future[List[Problem]] = {
    val pipeline = sendReceive ~> unmarshal[List[Problem]]
    val response = pipeline(
      Post(
        Uri.from(
          scheme = "http",
          host = hostname,
          path = "/myproblems",
          query = Query("auth" -> (token + suffix)))))

    response.onComplete {
      case Success(probs: List[Problem]) =>
        val out = new PrintStream("problems.csv")
        out.println("ID,Size,Operators")
        probs.map { p =>
          "%s,%s,\"%s\"".format(p.id, p.size, p.operators.mkString(", "))
        }.foreach(out.println)
        out.close()
        log.info("Problems written to problems.csv")
      case Success(unexpected) =>
        log.warning("The API call was successful but returned something unexpected: '{}'.", unexpected)
      case Failure(error) =>
        log.error(error, "Something went wrong.")
    }
    response
  }

  def train(tr: TrainRequest): Future[TrainingProblem] = {
    val pipeline = sendReceive ~> unmarshal[TrainingProblem]
    val response = pipeline(
      Post(
        Uri.from(
          scheme = "http",
          host = hostname,
          path = "/train",
          query = Query("auth" -> (token + suffix))), tr))

    response.onComplete {
      case Success(tp: TrainingProblem) =>
      case Success(unexpected) =>
        log.warning("The API call was successful but returned something unexpected: '{}'.", unexpected)
      case Failure(error) =>
        log.error(error, "Something went wrong.")
    }
    response
  }

  def shutdown(): Unit = {
    IO(Http).ask(Http.CloseAll)(1.second).await
    system.shutdown()
  }
}
