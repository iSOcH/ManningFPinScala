package ch.pascalschwarz.manningfpinscala.Chap09Parsers

// ex 09_00
trait MyParsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def optCount[A](p: Parser[A]): Parser[Int]

  /*
  "could the API support giving an explicit message...?"
  yes, I think this is needed as part of Parser[A]
   */
  def reqCount[A](p: Parser[A]): Parser[Int]

  def pairCount[A,B](p1: Parser[A], p2: Parser[B]): Parser[(Int, Int)]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]) = self.or(p, p2)
    def or[B>:A](p2: Parser[B]) = self.or(p, p2)
  }

  // problem with unneeded List[A] when just a its length is wanted
  def counter[A](p: Parser[A]): Parser[Int] // should be possible to implement more efficiently than using listOfN

  // questions regarding repetition, ParseError... no idea

  /*
  "does a|b and b|a mean the same thing? yes/no, what are consequences?"
  - when it's the same:
    - we need to try both parsers
    - if both succeed, we need to have information on which to prefer
      - otherwise, separate results from same input could be valid
  - not the same
    - we can stop after first successful parse
    - no separate information for preference needed (a is preferred in a|b)

  ... seems smarter to make them not the same
   */

  /*
  a|(b|c) and (a|b)|c the same?
  yes. this is implied by the previous decision
   */
}
