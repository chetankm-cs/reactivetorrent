import java.io.File
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

import bencode.Tokens.{BeDict, BeString, _}
import bencode.{BencodeDecoder, BencodeEncoder}
import org.apache.commons.codec.digest.DigestUtils
import play.api.libs.ws.StandaloneWSClient

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{Codec, Source}


case class Peer(peerId: String, host: String, port: Int)

class Tracker(file: File, wsClient: StandaloneWSClient) {

  def hexStringURLEncode(x: String): String = {
    x.grouped(2).toList.map("%" + _).mkString("")
  }

  def init(): Future[Unit] = {
    val source = Source.fromFile(file)(Codec(StandardCharsets.ISO_8859_1)).mkString

    val metaInfo = BencodeDecoder.decode(source).get.as[BeDict]
    val announce = metaInfo.d("announce").as[BeString]
    val info = metaInfo.d("info").as[BeDict]

    val encoded = BencodeEncoder.encode(info)
    val sha1 = DigestUtils.sha1Hex(encoded.getBytes(StandardCharsets.ISO_8859_1))
    val urlEncodedInfoHash = hexStringURLEncode(sha1)

    val peer_id = "9feffc0136104f3fa047"

    val url = s"$announce?peer_id=$peer_id&info_hash=$urlEncodedInfoHash&port=6881&left=10&downloaded=0&uploaded=0"
    wsClient.url(url).get().map {
      res =>
        val response = BencodeDecoder.decode(res.body).get.as[BeDict]
        val peerInfoList = response.d("peers").as[BeList].l.map(_.as[BeDict])

        val peers = peerInfoList.map {
          elem =>
            Peer(
              elem.d("peer id").as[BeString].s,
              elem.d("ip").as[BeString].s,
              elem.d("port").as[BeNumber].i.toInt
            )
        }

        val interval = response.d("interval").as[BeNumber].i

        peers.map {
          peer =>
            println(s"Starting PeerConnection for ${peer.host} ")
            val props = PeerConnection.props(new InetSocketAddress(peer.host, peer.port), hex2bytes(sha1), peer_id, peer.peerId)
            Main.system.actorOf(props)
        }
    }
  }

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }
}
