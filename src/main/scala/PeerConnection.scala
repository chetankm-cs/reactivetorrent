
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

import PeerConnection.HandShake
import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString

case class PeerConnection(inetSocketAddress: InetSocketAddress, infoHashHex: String, peerId: String) extends Actor {
  var interested = false
  var choking = false
  val client: ActorRef = context.system.actorOf(UdpClient.props(inetSocketAddress))

  client ! HandShake(infoHashHex, peerId).bytes

  override def receive: Receive = {
    case c: Tcp.Connected =>
      sender() ! HandShake(infoHashHex, peerId)
    case other =>
      println(s"Other $other")
  }
}

object PeerConnection {
  def props(inetSocketAddress: InetSocketAddress, infoHashHex: String, peerId: String) = Props(new PeerConnection(inetSocketAddress, infoHashHex, peerId))

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  trait Messages {
    def bytes: ByteString
  }

  case class HandShake(infoHashHex: String, peerId: String) extends Messages {
    lazy val bytes: ByteString = ByteString.newBuilder
      .putByte(19.toByte)
      .putBytes("BitTorrent protocol".getBytes())
      .putBytes(Stream.fill(8)(0.toByte).toArray)
      .putBytes(hex2bytes(infoHashHex))
      .putBytes(peerId.getBytes(StandardCharsets.ISO_8859_1))
      .result()
  }

}
