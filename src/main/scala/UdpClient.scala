import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.io.{IO, Udp, UdpConnected}
import akka.util.ByteString

class UdpClient(remote: InetSocketAddress) extends Actor with Stash {

  import context.system

  IO(UdpConnected) ! UdpConnected.Connect(self, remote)

  def receive = {
    case UdpConnected.Connected ⇒
      println("Connected " )
      context.become(ready(sender()))
      unstashAll()
    case other =>
      println(s"recieved $other")
      stash()
  }

  def ready(connection: ActorRef): Receive = {
    case UdpConnected.Received(data) ⇒
    // process data, send it on, etc.
      println("Recieved : " + data)
    case msg: ByteString ⇒
      println(s"Sending Data : $msg")
      connection ! UdpConnected.Send(msg)
    case UdpConnected.Disconnect ⇒
      connection ! UdpConnected.Disconnect
    case UdpConnected.Disconnected ⇒ context.stop(self)
  }
}
object UdpClient {
  def props(remote: InetSocketAddress) = Props(classOf[UdpClient], remote)
}



class Listener(nextActor: ActorRef) extends Actor {
  import context.system
  IO(Udp) ! Udp.Bind(self, new InetSocketAddress("localhost", 0))

  def receive = {
    case Udp.Bound(local) ⇒
      context.become(ready(sender()))
  }

  def ready(socket: ActorRef): Receive = {
    case Udp.Received(data, remote) ⇒
      val processed = // parse data etc., e.g. using PipelineStage
        socket ! Udp.Send(data, remote) // example server echoes back
      nextActor ! processed
    case Udp.Unbind  ⇒ socket ! Udp.Unbind
    case Udp.Unbound ⇒ context.stop(self)
  }
}