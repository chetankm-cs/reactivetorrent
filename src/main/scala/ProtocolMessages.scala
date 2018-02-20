import java.nio.ByteBuffer
import java.nio.charset.{Charset, StandardCharsets}

import akka.util.ByteString

import scala.util.Try

object ProtocolMessages {

  sealed trait Message

  sealed trait LengthEncodedMessage extends Message {
    val msgType: Int

    def payLoad: Option[List[Byte]]

    def length: Int = 4 + 1 + payLoad.fold(0)(_.length)
  }

  abstract class SingleLengthMessage(override val msgType: Int) extends LengthEncodedMessage {
    override def payLoad: Option[List[Byte]] = None
  }

  implicit class IntToByteArray(int: Int) {
    def toByteArray: Array[Byte] = ByteBuffer.allocate(4).putInt(int).array()
  }

  object Message {

    implicit class MessageLength(message: Message) {
      def byteLength: Int = message match {
        case h:HandShake =>  1 + 19 + 8 + 20 + 20
        case KeepAlive => 4
        case a: LengthEncodedMessage => a.length
      }
    }

    def encode(message: Message): ByteString = message match {

      case HandShake(infoHash, peerId) => ByteString.newBuilder
        .putByte(19.toByte)
        .putBytes("BitTorrent protocol".getBytes())
        .putBytes(Stream.fill(8)(0.toByte).toArray)
        .putBytes(infoHash)
        .putBytes(peerId.getBytes(StandardCharsets.ISO_8859_1))
        .result()

      case KeepAlive => ByteString(Array.ofDim[Byte](4))

      case a: LengthEncodedMessage =>
        ByteString.newBuilder
          .putBytes(a.length.toByteArray)
          .putByte(a.msgType.toByte)
          .putBytes(a.payLoad.map(_.toArray).getOrElse(Array.emptyByteArray))
          .result()
    }

    def decode(byteString: ByteString): Option[Message] = {
      byteString match {
        case handshake if byteString.head.toInt == 19 => Some(HandShake.fromByteString(handshake))
        case keepAlive if keepAlive.payloadLength.contains(0) => Some(KeepAlive)
        case lengthEncoded =>
          val msg = lengthEncoded.drop(5)
          lengthEncoded.msgType.flatMap {
            case Choke.msgType => Some(Choke)
            case UnChoke.msgType => Some(UnChoke)
            case Interested.msgType => Some(Interested)
            case NotInterested.msgType => Some(NotInterested)
            case Have.msgType => msg.headOption.map(b => Have(b))
            case BitField.msgType => Try(BitField(msg.toList)).toOption
            case Request.msgType =>
              Try {
                val integers: List[Int] = msg.grouped(4).take(3).map(_.asByteBuffer.getInt).toList
                Request(integers.head, integers(1), integers(2))
              }.toOption

            case Piece.msgType => Try {
              val (indexes, block) = msg.splitAt(8)
              val (index, begin) = indexes.splitAt(4)
              Piece(index.asByteBuffer.getInt, begin.asByteBuffer.getInt, block.toList)
            }.toOption
            case Cancel.msgType =>
              Try {
                val integers = msg.grouped(4).take(3).map(_.asByteBuffer.getInt).toList
                Cancel(integers.head, integers(1), integers(2))
              }.toOption
            case invalidMsgType =>
              println(s"Invalid msg type : $invalidMsgType for msg $lengthEncoded")
              None
          }

      }

    }

    implicit class ByteStringPayloadLengthExtractor(byteString: ByteString) {
      def payloadLength: Option[Int] = {
        if (byteString.length >= 4)
          Some(byteString.take(4).asByteBuffer.getInt)
        else
          None
      }

      def msgType: Option[Int] = {
        Try(byteString.apply(4)).toOption.map(_.toInt)
      }
    }

  }

  case class HandShake(infoHash: Array[Byte], peerId: String) extends Message {
    override def toString: String = s"HandShake(${ByteString.fromArray(infoHash)}, $peerId)"
  }

  object HandShake {
    def fromByteString(byteString: ByteString): Message = {
      val payload = byteString.drop(1 + 19 + 8)
      val (infoHash, peerId) = payload.splitAt(20)
      val peerIdDecoded = peerId.take(20).decodeString(StandardCharsets.ISO_8859_1)
      HandShake(infoHash.toArray, peerIdDecoded)
    }
  }

  case object KeepAlive extends Message

  case object Choke extends SingleLengthMessage(0)

  case object UnChoke extends SingleLengthMessage(1)

  case object Interested extends SingleLengthMessage(2)

  case object NotInterested extends SingleLengthMessage(3)

  case class Have(piece: Byte) extends LengthEncodedMessage {
    override val msgType: Int = Have.msgType

    override def payLoad: Option[List[Byte]] = Some(List(piece))
  }

  object Have {
    val msgType: Int = 4
  }

  case class BitField(bytes: List[Byte]) extends LengthEncodedMessage {
    override val msgType: Int = BitField.msgType

    override def payLoad: Option[List[Byte]] = Some(bytes)
  }

  object BitField {
    val msgType: Int = 5
  }

  case class Request(index: Int, begin: Int, requestLength: Int) extends
    LengthEncodedMessage {
    override val msgType: Int = Request.msgType

    override def payLoad: Option[List[Byte]] = Some((index.toByteArray ++ begin.toByteArray ++ requestLength.toByteArray).toList)
  }

  object Request {
    val msgType: Int = 6
  }

  case class Piece(index: Int, begin: Int, block: List[Byte]) extends LengthEncodedMessage() {
    override val msgType: Int = Piece.msgType

    override def payLoad: Option[List[Byte]] = Some((index.toByteArray ++ begin.toByteArray ++ block).toList)
  }

  object Piece {
    val msgType: Int = 7
  }

  case class Cancel(index: Int, begin: Int, requestLength: Int) extends LengthEncodedMessage {
    override val msgType: Int = Cancel.msgType

    override def payLoad: Option[List[Byte]] = Some((index.toByteArray ++ begin.toByteArray ++ requestLength.toByteArray).toList)
  }

  object Cancel {
    val msgType: Int = 8
  }

}