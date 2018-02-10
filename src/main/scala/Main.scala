import java.nio.charset.StandardCharsets

import bencode.BencodeDecoder

import scala.io.{Codec, Source}

object Main extends App {
  val file = getClass.getResource("theory_of_computation.torrent").getFile
  val source = Source.fromFile(file)(Codec(StandardCharsets.ISO_8859_1)).mkString
  val res = BencodeDecoder.decode(source)
  println(res)
}
