package ch.pascalschwarz.manningfpinscala.Chap09Parsers

import ch.pascalschwarz.manningfpinscala.Chap09Parsers.ParserTypes.Result

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  //def exCountA = map(many(char('a')))(_.size)
  //def exCountA = char('a').many.map(_.length)
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def regex(r: Regex): Parser[String]

  def succeed[A](a: A) = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]
  def exCountA = char('a').many.slice.map(_.length)

  def opt[A](p: Parser[A]): Parser[Option[A]] = (p map (Some(_))) | succeed(None)
  def many[A](p: Parser[A], separator: Parser[_]): Parser[List[A]] = many1(p, separator) | succeed(List())
  def many1[A](p: Parser[A], separator: Parser[_]): Parser[List[A]] = map2(p, (separator *> p).many)(_ :: _)

  def skipL[A](pl: Parser[_], pr: Parser[A]): Parser[A] = (pl.slice ** pr).map(_._2)
  def skipR[A](pl: Parser[A], pr: Parser[_]): Parser[A] = (pl ** pr.slice).map(_._1)
  def surround[A](start: Parser[_], end: Parser[_])(content: => Parser[A]): Parser[A] = start *> content <* end

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  val spaces = "\\s*".r.slice
  def token[A](p: Parser[A]): Parser[A] = p.attempt <* spaces

  val eof = regex("\\z".r).label("unexpected trailing characters")
  def root[A](p: Parser[A]) = p <* eof

  // ex 09_01
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = (p ** p2).map(f.tupled)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  val aAndBCount: Parser[(Int,Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

  // ex 09_03
  def many[A](p: Parser[A]): Parser[List[A]] = {
    // only if p can parse `many` will be called again
    map2(p, p.many)(_ :: _) or succeed(List())
  }

  // ex09_04
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = (n, p) match {
    case (0, _) => succeed(List())
    case (_, _) => map2(p, listOfN(n-1, p))(_ :: _)
  }

  // ex09_05
  /*
  def lzy[A](p: => Parser[A]): Parser[A] // seems unnecessary... don't know of any case where we want map2 to be strict
  def manyLzy[A](p: Parser[A]): Parser[List[A]] = {
    // only if p can parse `many` will be called again
    map2(p, lzy(p.many))(_ :: _) or succeed(List())
  }*/

  // ex09_06
  val int: Parser[Int] = regex("[0-9]+".r).map(_.toInt)
  val ctxSens: Parser[List[Char]] = int.flatMap(char('a').listOfN)

  // ex09_07
  def product[A,B](pa: Parser[A], pb: => Parser[B]): Parser[(A,B)] = pa.flatMap(a => pb map (a -> _))
  def map2ByFlatmap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = p flatMap(a => p2 map (f(a, _)))
  // for-comprehension looks much better

  // ex09_08
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p flatMap (a => succeed(f(a)))

  def attempt[A](p: Parser[A]): Parser[A]
  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]) = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]) = self.or(p, p2)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def map[B](f: A => B) = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)

    def many(separator: Parser[_]): Parser[List[A]] = self.many(p, separator)
    def many1(separator: Parser[_]): Parser[List[A]] = self.many1(p, separator)

    def product[B](pb: => Parser[B]): Parser[(A,B)] = self.product(p, pb)
    def **[B](pb: => Parser[B]): Parser[(A,B)]= self.product(p, pb)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def opt: Parser[Option[A]] = self.opt(p)
    def attempt: Parser[A] = self.attempt(p)
    def token: Parser[A] = self.token(p)

    def *>[B](pr: Parser[B]): Parser[B] = self.skipL(p, pr)
    def <*(pr: Parser[_]): Parser[A] = self.skipR(p, pr)

    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
  }

  object Laws {
    import ch.pascalschwarz.manningfpinscala.Chap08Testing._
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(x => x))(in)

    def succeedLaw(in: Gen[String]) = Prop.forAll(in)(s => run(succeed(1))(s) == Right(1))

    // ex09_02
    def productLawFirst(in: Gen[String]) = {
      val aParser = char('a').many
      equal(aParser, product(aParser, succeed('x')).map(_._1))(in)
    }
    def productLawSecond(in: Gen[String]) = {
      val aParser = char('a').many
      equal(aParser, product(succeed('x'), aParser).map(_._2))(in)
    }
    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
      run(label(msg)(p))(input) match {
        case Left(e) => errorMessage(e) == msg
        case _ => true
      }
    }
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset+1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset+1).lastIndexOf('\n') match {
    case -1 => offset+1
    case lineStart => offset - lineStart
  }
  def advanceBy(n: Int): Location = copy(offset = offset + n)
  lazy val cur: String = input.substring(offset)
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError = copy(stack = loc -> msg :: stack)
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
  def latestLoc: Option[Location] = latest map (_._1)
  def latest: Option[(Location, String)] = stack.lastOption
}