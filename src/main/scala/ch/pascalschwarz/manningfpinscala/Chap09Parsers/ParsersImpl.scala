package ch.pascalschwarz.manningfpinscala.Chap09Parsers

import ch.pascalschwarz.manningfpinscala.Chap09Parsers.ParserTypes.{Failure, Success, Parser}

import scala.util.matching.Regex

object ParserTypes {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object ParserImpl extends Parsers[Parser] {
  override def string(s: String): Parser[String] = loc => {
    if (loc.input.startsWith(s)) Success(s, s.length)
    else Failure(ParseError(List(loc -> s"$s not found")))
  }

  override def regex(r: Regex): Parser[String] = loc => {
    r.findFirstIn(loc.input) match {
      case Some(res) => Success(res, res.length)
      case _ => Failure(ParseError(List(loc -> s"$r did not match")))
    }
  }

  override def succeed[A](a: A): Parser[A] = loc => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] = loc => {
    p(loc) match {
      case Success(_, n) => Success(loc.input.take(n), n)
      case f:Failure => f
    }
  }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = l =>
    p(l).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = l =>
    p(l).mapError(_.push(l, msg))
}