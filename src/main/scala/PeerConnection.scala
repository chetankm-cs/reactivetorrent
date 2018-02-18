
import java.net.InetSocketAddress

import ProtocolMessages.{HandShake, Message}
import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString

case class PeerConnection(inetSocketAddress: InetSocketAddress, infoHashHex: Array[Byte], peerId: String) extends Actor {
  var interested = false
  var choking = false
  val client: ActorRef = context.system.actorOf(TcpClient.props(inetSocketAddress, self))
  val tag: String = inetSocketAddress.getHostName

  def protocolHandler: Receive = {
    case "connection closed" =>
      println(s"Closing connection for $tag")
      context stop self
    case data: ByteString =>
      val msg = Message.decode(data)
      println(s"msg received $msg for $tag")
  }

  override def receive: Receive = {
    case c: Tcp.Connected =>
      println(s"${inetSocketAddress.getHostName} connected")
      sender() ! Message.encode(HandShake(infoHashHex, peerId))
      context become protocolHandler
    case "connection closed" =>
      println(s"Closing connection for $tag")
      context stop self
  }
}

object PeerConnection {
  def props(inetSocketAddress: InetSocketAddress, infoHash: Array[Byte], peerId: String) = Props(new PeerConnection(inetSocketAddress, infoHash, peerId))
}
