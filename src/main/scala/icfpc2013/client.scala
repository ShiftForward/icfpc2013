package icfpc2013

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.io.IO
import akka.event.Logging
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.http.Uri._
import spray.httpx.SprayJsonSupport._
import java.io.PrintStream
import scala.concurrent.Future
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

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

  def post[Req: Marshaller, Resp: Unmarshaller: Manifest](endpoint: String, body: Req): Future[Resp] = {
    val pipeline = sendReceive ~> unmarshal[Resp]
    val response = pipeline(
      Post(
        Uri.from(
          scheme = "http",
          host = hostname,
          path = "/" + endpoint,
          query = Query("auth" -> (token + suffix)))))

    response.onComplete {
      case Success(resp: Resp) =>
      case Success(unexpected) =>
        log.warning("The API call was successful but returned something unexpected: '{}'.", unexpected)
      case Failure(error) =>
        log.error(error, "Something went wrong.")
    }
    response
  }

  def status = post[String, Status]("status", "")

  def problems = post[String, List[Problem]]("problems", "").map { probs =>
    val out = new PrintStream("problems.csv")
    out.println("ID,Size,Operators")
    probs.map { p =>
      "%s,%s,\"%s\"".format(p.id, p.size, p.operators.mkString(", "))
    }.foreach(out.println)
    out.close()
    log.info("Problems written to problems.csv")
    probs
  }

  def train = post[TrainRequest, TrainingProblem]("train", _: TrainRequest)
  def guess = post[Guess, GuessResponse]("guess", _: Guess)

  def shutdown(): Unit = {
    IO(Http).ask(Http.CloseAll)(1.second).await
    system.shutdown()
  }
}
