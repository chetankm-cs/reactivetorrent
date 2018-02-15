package bencode

object Tokens {

  trait BeElem

  case class BeNumber(i: Long) extends BeElem {
    override def toString: String = i.toString
  }

  case class BeString(s: String) extends BeElem {
    override def toString: String = s.toString
  }

  case class BeList(l: List[BeElem]) extends BeElem {
    override def toString: String = l.toString()
  }

  case class BeDict(d: Map[BeString, BeElem]) extends BeElem {
    override def toString: String = d.toString()
  }

  implicit def liftString(s: String): BeString = BeString(s)

  implicit def liftNumber(i: Long): BeNumber = BeNumber(i)

  implicit class BeElemExtractor(beElem: BeElem) {
    def as[T <: BeElem]: T = beElem match {
      case t: T => t
      case other => throw new IllegalArgumentException(s"found $other")
    }
  }

}
