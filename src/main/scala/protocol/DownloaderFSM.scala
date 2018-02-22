package protocol

import akka.actor.Actor
import protocol.ProtocolMessages.{BitField, Message, Piece, UnChoke}
import DownloaderFSM._

case class FileStats(pieceLength: Int, fileLength: Int) {
  def pieces: Int = Math.ceil(fileLength.toDouble / pieceLength).toInt

  def pieceLength(index: Int) = {
    val x = fileLength - (pieceLength * index)
    if (x < pieceLength) x else pieceLength
  }

  def blocksInPiece(index: Int): Int = {
    val length = pieceLength(index)
    Math.ceil(length.toDouble / blockSize).toInt
  }

  def blockLength(index: Int, blockIndex: Int): Int = {
    val x = pieceLength - (blockIndex * blockSize)
    if (x < blockSize) x else blockSize
  }
}

class DownloaderFSM(pieceLength: Int, fileLength: Int) extends Actor {
  val fileStats = FileStats(pieceLength, fileLength)
  val blocksPerPiece: Int = pieceLength / blockSize // assuming multiple of 2^16
  var downloadedPieces: Map[Int, List[Byte]] = Map.empty
  var availablePieces: Set[Int] = Set()
  var inProgressPieces: Map[Int, Map[Int, List[Byte]]] = Map.empty

  def isSet(index: Int, byte: List[Byte]): Boolean = {
    val bIndex = index / 8
    val bOffSet = index % 8
    if (bIndex > byte.length - 1) false
    else
      (byte(bIndex) & (1 << bOffSet)) > 1
  }


  def handleMessage(message: Message): Unit = message match {
    case BitField(bytes) =>
      availablePieces = (1 to fileStats.pieces).filter(isSet(_, bytes)).toSet
    case UnChoke =>
    //request
    case Piece(index, begin, block) =>
      if (begin % blockSize != 0) println(s"Invalid begin $begin")
      val blockIndex: Int = begin / blockSize
      val pieceProgress = inProgressPieces.getOrElse(index, Map.empty)
      val updatedProgress: Map[Int, List[Byte]] = pieceProgress.updated(blockIndex, block)

      if (fileStats.blocksInPiece(index) == updatedProgress.size) {
        val pieceData = updatedProgress.toSeq.sortBy(_._1).flatMap(_._2).toList
        downloadedPieces = downloadedPieces.updated(index, pieceData)
        inProgressPieces = inProgressPieces - index
        println(s"Downloaded Piece $index")
      } else {
        inProgressPieces = inProgressPieces.updated(index, updatedProgress)
      }
    // updateState
    case other => ()
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
  val blockSize: Int = 1 << 16

}
