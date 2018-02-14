import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import bencode.{BencodeDecoder, BencodeEncoder}
import bencode.Tokens._
import org.apache.commons.codec.digest.DigestUtils
import play.api.libs.ws.StandaloneWSClient
import play.api.libs.ws.ahc.StandaloneAhcWSClient

import scala.concurrent.Future
import scala.io.{Codec, Source}
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  implicit val system: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  val wsClient = StandaloneAhcWSClient()

  val file = getClass.getResource("toc.torrent").getFile
  val source = Source.fromFile(file)(Codec(StandardCharsets.ISO_8859_1)).mkString
  val metaInfo = (BencodeDecoder.decode(source).get match {
    case d: BeDict => Some(d)
    case _ => None
  }).get

  val announce = (metaInfo.d(BeString("announce")) match {
    case string: BeString => Some(string)
    case _ => None
  }).get

  val info = (metaInfo.d(BeString("info")) match {
    case d: BeDict => Some(d)
    case _ => None
  }).get

  val encoded = BencodeEncoder.encode(info)
  val sha1 = DigestUtils.sha1Hex(encoded.getBytes(StandardCharsets.ISO_8859_1))
  val urlEncoded = URLEncoder.encode(sha1.grouped(2).map("%" + _).mkString, "UTF-8")


  call(wsClient, announce.s, hexStringURLEncode(sha1))
    .andThen { case _ => wsClient.close() }
    .andThen { case _ => system.terminate() }

  def call(wsClient: StandaloneWSClient, announce: String, info: String): Future[String] = {
    val url = s"$announce?peer_id=9feffc0136104f3fa047&info_hash=$info&port=6881&left=10&downloaded=0&uploaded=0"
    println(s"Url: $url")
    wsClient.url(url).get().map {
      res =>
        println(BencodeDecoder.decode(res.body))
        res.body
    }
  }

  def hexStringURLEncode(x: String) = {
    x.grouped(2).toList.map("%" + _).mkString("")
  }
}

