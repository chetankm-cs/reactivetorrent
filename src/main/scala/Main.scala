import java.io.File

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.libs.ws.ahc.StandaloneAhcWSClient

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  val wsClient = StandaloneAhcWSClient()

  val file = getClass.getResource("toc.torrent").getFile
  val tracker = new Tracker(new File(file), wsClient)
  println(Await.result(tracker.init(), Duration.Inf))
}

