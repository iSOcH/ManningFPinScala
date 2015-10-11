package ch.pascalschwarz.manningfpinscala.Chap09Parsers

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](p: Parser[A])(f: A => B): Parser[B]
  //def exCountA = map(many(char('a')))(_.size)
  //def exCountA = char('a').many.map(_.length)

  def succeed[A](a: A) = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]
  def exCountA = char('a').many.slice.map(_.length)

  def many1[A](p: Parser[A]): Parser[List[A]]
  def product[A,B](pa: Parser[A], pb: Parser[B]): Parser[(A,B)]


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]) = self.or(p, p2)
    def or[B>:A](p2: Parser[B]) = self.or(p, p2)

    def map[B](f: A => B) = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
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
  }
}
