package icfpc2013

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.io.IO
import akka.event.Logging
import java.io.PrintStream
import scala.concurrent.Future
import scala.concurrent.duration._
import spray.can.Http
import spray.client.pipelining._
import scala.collection.mutable
import spray.http._
import spray.http.Uri._
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.Unmarshaller

object Client {
  import spray.util._
  import JsonApi._

  val token = "YOUR_TOKEN_HERE"
  val suffix = "vpsH1H"
  val hostname = "icfpc2013.cloudapp.net"

  val windowWidth = 21000 // 1 second tolerance
  val windowAllowance = 5
  var windowQueue = mutable.Queue[Long]()

  def currTime = System.currentTimeMillis()

  def clearWindowFor(n: Int) {
    while(windowQueue.nonEmpty &&
      windowQueue.head + windowWidth < currTime) windowQueue.dequeue()

    if(windowAllowance < windowQueue.size + n) {
      val toWait = windowQueue.head + windowWidth - currTime
      println("Waiting %d seconds before making the next request...".format(toWait / 1000))
      Thread.sleep(toWait)
      clearWindowFor(n - 1)
    }
  }

  implicit lazy val system = ActorSystem()
  import system.dispatcher
  val log = Logging(system, getClass)

  implicit val timeout = 10 seconds

  def post[Req: Marshaller, Resp: Unmarshaller: Manifest](endpoint: String, body: Req): Future[Resp] = {
    clearWindowFor(1)

    val pipeline = sendReceive ~> unmarshal[Resp]
    val response = pipeline(
      Post(
        Uri.from(
          scheme = "http",
          host = hostname,
          path = "/" + endpoint,
          query = Query("auth" -> (token + suffix))), body))

    windowQueue.enqueue(currTime)

    response.onFailure {
      case e: Exception => log.error(e, "Something went wrong.")
    }
    response
  }

  def status = post[String, Status]("status", "")

  def problems = post[String, List[Problem]]("myproblems", "").map { probs =>
    val out = new PrintStream("problems.csv")
    out.println("ID,Size,Operators,Solved")
    probs.map { p =>
      "%s,%s,\"%s\",%s".format(
        p.id,
        p.size, p.operators.mkString(", "),
        p.solved.map(_.toString).getOrElse(""))
    }.foreach(out.println)
    out.close()
    log.info("Problems written to problems.csv")
    probs
  }

  def train = post[TrainRequest, TrainingProblem]("train", _: TrainRequest)
  def eval = post[EvalRequest, EvalResponse]("eval", _: EvalRequest)
  def guess = post[Guess, GuessResponse]("guess", _: Guess)

  def shutdown(): Unit = {
    IO(Http).ask(Http.CloseAll)(1.second).await
    system.shutdown()
  }
}
