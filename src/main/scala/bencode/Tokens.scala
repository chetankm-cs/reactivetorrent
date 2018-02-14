package bencode

object Tokens {

  trait BeElem

  case class BeNumber(i: Long) extends BeElem

  case class BeString(s: String) extends BeElem // ISO_8859_1 encoded
  case class BeList(l: List[BeElem]) extends BeElem

  case class BeDict(d: Map[BeString, BeElem]) extends BeElem
}
