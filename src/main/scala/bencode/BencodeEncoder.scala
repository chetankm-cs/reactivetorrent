package bencode

import bencode.Tokens._

object BencodeEncoder {
  def encode(elem: BeElem): String = elem match {
    case BeString(s) => s"${s.length}:$s"
    case BeDict(dict) =>
      val elems = dict.toSeq.sortBy(_._1.s)
      val encoded = elems.map {
        case (k, v) => encode(k) + encode(v)
      }
      s"d${encoded.mkString}e"
    case BeNumber(i) => s"i${i.toString}e"
    case BeList(l) => s"l${l.map(encode).mkString}e"
  }
}
