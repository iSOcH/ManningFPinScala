package ch.pascalschwarz.manningfpinscala.Chap09Parsers

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]
  //def exCountA = map(many(char('a')))(_.size)
  //def exCountA = char('a').many.map(_.length)

  def succeed[A](a: A) = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]
  def exCountA = char('a').many.slice.map(_.length)

  def product[A,B](pa: Parser[A], pb: Parser[B]): Parser[(A,B)]

  // ex 09_01
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = (p ** p2).map(f.tupled)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)

  val aAndBCount: Parser[(Int,Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

  // ex 09_03
  def many[A](p: Parser[A]): Parser[List[A]] = {
    // only if p can parse `many` will be called again
    map2(p, p.many)(_ :: _) or succeed(List())
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]) = self.or(p, p2)
    def or[B>:A](p2: Parser[B]) = self.or(p, p2)

    def map[B](f: A => B) = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)

    def product[B](pb: Parser[B]): Parser[(A,B)] = self.product(p, pb)
    def **[B](pb: Parser[B]): Parser[(A,B)]= self.product(p, pb)
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
  }
}
