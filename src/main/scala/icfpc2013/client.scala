package icfpc2013

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.io.IO
import akka.event.Logging
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.http.Uri._
import spray.httpx.SprayJsonSupport._
import spray.util._

import JsonApi._

object Client extends App {
  val token = "INSERT_TOKEN_HERE"
  val suffix = "vpsH1H"
  val hostname = "icfpc2013.cloudapp.net"

  implicit val system = ActorSystem()
  import system.dispatcher
  val log = Logging(system, getClass)

  status

  def status = {
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
        log.info(status.toString)
        shutdown()
      case Success(unexpected) =>
        log.warning("The API call was successful but returned something unexpected: '{}'.", unexpected)
        shutdown()
      case Failure(error) =>
        log.error(error, "Something went wrong.")
        shutdown()
    }
  }

  def shutdown(): Unit = {
    IO(Http).ask(Http.CloseAll)(1.second).await
    system.shutdown()
  }
}
