package ch.pascalschwarz.manningfpinscala.Chap09Parsers

import ch.pascalschwarz.manningfpinscala.Chap09Parsers.ParserTypes.{Failure, Success, Parser}

import scala.util.matching.Regex

object ParserTypes {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }
    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a, n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, committed: Boolean) extends Result[Nothing]
}

object ParserImpl extends Parsers[Parser] {
  override def string(s: String): Parser[String] = loc => {
    if (loc.cur.startsWith(s)) Success(s, s.length)
    else Failure(ParseError(List(loc -> s"$s not found")), loc.cur.startsWith(s.head.toString))
  }

  override def regex(r: Regex): Parser[String] = loc => {
    r.findFirstIn(loc.cur) match {
      case Some(res) => Success(res, res.length)
      case _ => Failure(ParseError(List(loc -> s"$r did not match")), false)
    }
  }

  override def succeed[A](a: A): Parser[A] = loc => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] = loc => {
    p(loc) match {
      case Success(_, n) => Success(loc.cur.take(n), n)
      case f:Failure => f
    }
  }

  override def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = l =>
    p(l).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = l =>
    p(l).mapError(_.push(l, msg))

  override def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s =>
    x(s) match {
      case Failure(e, false) => y(s)
      case r => r
    }

  override def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] = s =>
    f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n))
                            .addCommit(n != 0)
                            .advanceSuccess(n)
      case e@Failure(_,_) => e
    }


  // ex 09_15
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }
  }
  override def errorLocation(pe: ParseError): Location = ???
  override def errorMessage(pe: ParseError): String = ???

}