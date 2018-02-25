package protocol

import java.nio.file.{Files, Paths}

import akka.actor.{Actor, ActorRef, Props}
import akka.util.ByteString
import protocol.DownloaderFSM._
import protocol.ProtocolMessages.{BitField, Interested, Message, Piece, Request, UnChoke}

case class FileStats(pieceSize: Int, fileLength: Int) {
  def pieces: Int = Math.ceil(fileLength.toDouble / pieceSize).toInt

  def pieceLength(index: Int): Int = {
    val x = fileLength - (pieceSize * index)
    if (x < pieceSize) x else pieceSize
  }

  def blocksInPiece(index: Int): Int = {
    val length = pieceLength(index)
    Math.ceil(length.toDouble / blockSize).toInt
  }

  def blockLength(index: Int, blockIndex: Int): Int = {
    val x = pieceLength(index) - (blockIndex * blockSize)
    if (x < blockSize) x else blockSize
  }
}

class DownloaderFSM(pieceLength: Int, fileLength: Int, connection: ActorRef) extends Actor {
  val fileStats = FileStats(pieceLength, fileLength)
  val blocksPerPiece: Int = pieceLength / blockSize // assuming multiple of 2^16
  var downloadedPieces: Map[Int, ByteString] = Map.empty
  var availablePieces: Set[Int] = Set()
  var inProgressPieces: Map[Int, Map[Int, ByteString]] = Map.empty

  def isSet(index: Int, byte: ByteString): Boolean = {
    val bIndex = index / 8
    val bOffSet = index % 8
    if (bIndex > byte.length - 1) false
    else
      (byte(bIndex) & (1 << bOffSet)) >= 1
  }

  def sendRequest(): Unit = {
    val toDownload: Set[Int] = availablePieces -- downloadedPieces.keySet
    if (toDownload.isEmpty) {
      val array = downloadedPieces.toList.sortBy(_._1).flatMap(_._2).toArray
      Files.write(Paths.get("/Users/workstation/Downloads/toc.pdf"), array)
      println(s"All Bytes: ${array.length} downloaded")
    } else {
      val mayBeNextBlock = inProgressPieces.find {
        case (piece, pieceProgress) =>
          fileStats.blocksInPiece(piece) != pieceProgress.size
      }.map {
        case (piece, pieceProgress) =>
          (piece, pieceProgress.size) // download next block
      }
      val (pieceToDownload, blockIndex) = mayBeNextBlock.getOrElse((toDownload.min, 0))
      val request = Request(pieceToDownload, blockIndex * blockSize, fileStats.blockLength(pieceToDownload, blockIndex))
      val msg = Message.encode(request)
      println(s"Sending Request $msg")
      connection ! msg
    }
  }

  def handleMessage(message: Message): Unit = message match {
    case BitField(bytes) =>
      availablePieces = (0 to fileStats.pieces).filter(isSet(_, bytes)).toSet
      if (availablePieces.nonEmpty) {
        val msg = Message.encode(Interested)
        println(s"Sending Interested: $msg")
        connection ! msg
      }
    case UnChoke =>
      sendRequest()
    case Piece(index, begin, block) =>
      if (begin % blockSize != 0) println(s"Invalid begin $begin")
      val blockIndex: Int = begin / blockSize
      val pieceProgress = inProgressPieces.getOrElse(index, Map.empty)
      val updatedProgress: Map[Int, ByteString] = pieceProgress.updated(blockIndex, ByteString(block.toArray))

      if (fileStats.blocksInPiece(index) == updatedProgress.size) {
        val pieceData = updatedProgress.toSeq.sortBy(_._1).flatMap(_._2).toList
        downloadedPieces = downloadedPieces.updated(index, ByteString(pieceData.toArray))
        inProgressPieces = inProgressPieces - index
        println(s"Downloaded: Piece $index")
      } else {
        println(s"Downloaded: Piece $index, Block $blockIndex")
        inProgressPieces = inProgressPieces.updated(index, updatedProgress)
      }
      sendRequest()
    case other =>
      println(s"invalid msg: $other")
  }

  override def receive: Receive = {
    case m: Message =>
      handleMessage(m)
    case other =>
      println("invalid msg")
  }
}

case class ConnectionState(choked: Boolean = true, interested: Boolean = false)

object DownloaderFSM {
  val blockSize: Int = 1 << 14

  def props(pieceLength: Int, fileLength: Int, connectionRef: ActorRef) = Props(new DownloaderFSM(pieceLength, fileLength, connectionRef))
}
