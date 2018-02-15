import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString

object TcpClient {
  def props(remote: InetSocketAddress, replies: ActorRef) = Props(classOf[TcpClient], remote, replies)
}

class TcpClient(remote: InetSocketAddress, listener: ActorRef) extends Actor {

  import akka.io.Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)
  println(s"Connecting to remote $remote")

  def connectedBehaviour(connection: ActorRef): PartialFunction[Any, Unit] = {
    case data: ByteString ⇒
      connection ! Write(data)
    case CommandFailed(w: Write) ⇒
      // O/S buffer was full
      listener ! "write failed"
    case Received(data) ⇒
      listener ! data
    case "close" ⇒
      connection ! Close
    case _: ConnectionClosed ⇒
      listener ! "connection closed"
      context stop self
  }

  def receive = {
    case CommandFailed(_: Connect) ⇒
      listener ! "connect failed"
      context stop self

    case c@Connected(remote, local) ⇒
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context become connectedBehaviour(connection)
  }
}