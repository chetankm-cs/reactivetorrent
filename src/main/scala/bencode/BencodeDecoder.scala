package bencode

import scala.util.parsing.combinator.{ImplicitConversions, Parsers}
import Tokens._

object BencodeDecoder extends Parsers with ImplicitConversions {

  implicit def strToInput(in: String): Input =
    new scala.util.parsing.input.CharArrayReader(in.toCharArray)

  type Elem = Char

  def decode(in: String): ParseResult[BeElem] = doc(in)

  lazy val doc: Parser[BeElem] = number.map(BeNumber) | string.map(BeString) | list.map(BeList) | dict.map(BeDict)

  lazy val number: Parser[Long] = 'i' ~> int <~ 'e'
  lazy val int: Parser[Long] = (digits ^^ (x => x.mkString.toLong)) | ('-' ~> digits ^^ (x => x.mkString.toLong * -1))
  lazy val digits: Parser[List[Char]] = rep1(digit)
  lazy val digit: Parser[Char] = elem("digit", c => c >= '0' && c <= '9')

  lazy val string: Parser[String] = '0' <~ ':' ^^ (_ => "") | len >> stringN
  lazy val len: Parser[Long] = int <~ ':'
  def stringN(n: Long): Parser[String] = repN(n.toInt, char).map(_.mkString)

  lazy val char: Parser[Char] = elem("any char", _ => true)

  lazy val list: Parser[List[BeElem]] = 'l' ~> rep(doc) <~ 'e'

  lazy val dict: Parser[Map[BeString, BeElem]] = 'd' ~> rep1(pair) <~ 'e' ^^ (xs => Map(xs: _*)) | 'd' ~ 'e' ^^ { _ => Map() }
  lazy val pair: Parser[(BeString, BeElem)] = string ~ doc ^^ { case x ~ y => (BeString(x), y) }
}


