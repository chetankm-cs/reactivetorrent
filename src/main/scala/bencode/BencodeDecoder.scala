package bencode

import scala.util.parsing.combinator.{ImplicitConversions, Parsers}

object BencodeDecoder extends Parsers with ImplicitConversions {

  trait BeElem

  case class BeNumber(i: Long) extends BeElem

  case class BeString(s: String) extends BeElem // US_ASCII encoded
  case class BeList(l: List[BeElem]) extends BeElem

  case class BeDict(d: Map[BeString, BeElem]) extends BeElem


  implicit def strToInput(in: String): Input =
    new scala.util.parsing.input.CharArrayReader(in.toCharArray)

  type Elem = Char

  def decode(in: String): ParseResult[Any] = doc(in)

  lazy val doc: Parser[BeElem] = number.map(BeNumber) | string.map(BeString) | list.map(BeList) | dict.map(BeDict)

  // Numbers i-10e -> -10i
  lazy val number: Parser[Long] = 'i' ~> int <~ 'e'
  lazy val int: Parser[Long] = (digits ^^ (x => x.mkString.toLong)) | ('-' ~> digits ^^ (x => x.mkString.toLong * -1))
  lazy val digits: Parser[List[Char]] = rep1(digit)
  lazy val digit: Parser[Char] = elem("digit", c => c >= '0' && c <= '9')

  lazy val string: Parser[String] = '0' <~ ':' ^^ (_ => "") | len >> stringN
  lazy val len: Parser[Long] = int <~ ':'
  def stringN(n: Long): Parser[String] = repN(n.toInt, char).map(_.mkString)

  lazy val char: Parser[Char] = elem("any char", _ => true)

  // Lists li1ei2ee -> [1, 2]
  lazy val list: Parser[List[BeElem]] = 'l' ~> rep(doc) <~ 'e'

  // Dictionaries d3:fooi1ee -> { "foo": 1 }
  lazy val dict: Parser[Map[BeString, BeElem]] = 'd' ~> rep1(pair) <~ 'e' ^^ (xs => Map(xs: _*)) | 'd' ~ 'e' ^^ { _ => Map() }
  lazy val pair: Parser[(BeString, BeElem)] = string ~ doc ^^ { case x ~ y => (BeString(x), y) }
}

