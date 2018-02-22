package protocol


import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString
import network.TcpClient
import protocol.ProtocolMessages.{HandShake, KeepAlive, Message}

case class PeerConnection(inetSocketAddress: InetSocketAddress, infoHash: Array[Byte], peerId: String, expectedPeerId: String)
  extends Actor {

  var interested = false
  var choking = false
  val client: ActorRef = context.system.actorOf(TcpClient.props(inetSocketAddress, self))
  val tag: String = inetSocketAddress.getHostName
  var prevData: ByteString = ByteString.empty

  def protocolHandler: Receive = {
    case "connection closed" =>
      println(s"Closing connection for $tag")
      context stop self

    case rawData: ByteString =>
      val data = prevData ++ rawData
      val msgOpt = Message.decode(data)
      prevData = msgOpt.fold(prevData ++ rawData) {
        msg => rawData.drop(msg.byteLength)
      }

      if (msgOpt.isEmpty) println(s"Invalid msg: $data received for $tag ")

      msgOpt foreach {
        msg =>
          println(s"Handling msg $msg")
          handleMessage(msg)
      }
  }

  def handleMessage(message: Message): Unit =
    message match {
      case KeepAlive => ()
      case HandShake(receivedInfoHash, receivedPeerId) =>
        if (!receivedInfoHash.sameElements(receivedInfoHash) || receivedPeerId != expectedPeerId) {
          println(s"InfoHash:: received:$receivedInfoHash expected: $infoHash")
          println(s"PeerId:: received:$receivedPeerId expected: $expectedPeerId")
          client ! "close"
        }
      case other => "hello"
    }

  override def receive: Receive = {
    case c: Tcp.Connected =>
      println(s"${inetSocketAddress.getHostName} connected")
      sender() ! Message.encode(HandShake(infoHash, peerId))
      context become protocolHandler
    case "connection closed" =>
      println(s"Closing connection for $tag")
      context stop self
  }
}

object PeerConnection {
  def props(inetSocketAddress: InetSocketAddress, infoHash: Array[Byte], peerId: String, expectedPeerId: String) = Props(new PeerConnection(inetSocketAddress, infoHash, peerId, expectedPeerId))
}
