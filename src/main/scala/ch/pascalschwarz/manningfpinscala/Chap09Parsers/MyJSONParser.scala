package ch.pascalschwarz.manningfpinscala.Chap09Parsers

import ch.pascalschwarz.manningfpinscala.Chap09Parsers.JSON._

// ex 09_09
object MyJSONParser {
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice

    val jsonNull: Parser[JNull.type] = string("null").slice.map(_ => JNull)

    // TODO: NaN, Infinity
    val jsonNumber: Parser[JNumber] = (regex("-?[0-9]+\\.?[0-9]*".r) or regex("-?\\.[0-9]+".r)) map (d => JNumber(d.toDouble))

    // we might want to handle toBoolean error and result in parse-fail... but it should not fail here
    val jsonBool: Parser[JBool] = (string("true") | string("false")) map (b => JBool(b.toBoolean))


    object stringParsing {
      val unicode:Parser[String] =  (string("\\u").slice ** regex("[0-9a-fA-F]{4}".r)) map (u => Character.toChars(Integer.parseInt(u._2, 16)).toString)
      val escapedChar:Parser[String] = (char('\\').slice ** regex("."r)) map (_._2.charAt(1).toString)
      val regularString:Parser[String] = regex("[^\"\\]*"r)
    }
    val jsonString: Parser[JString] =
      (char('"') **
        (stringParsing.unicode | stringParsing.escapedChar | stringParsing.regularString).many **
      char('"'))
        .map(s => JString(s._1._2.mkString))

    val jsonArray: Parser[JArray] =
      (char('[') **
      jsonParser(P).many(char(',')) **
      char(']'))
      .map(l => JArray(l._1._2.to[IndexedSeq]))

    val jsonObject: Parser[JObject] =
      (char('{') **
        (jsonString ** char(':') ** jsonParser(P)).many(char(',')) **
      char('}'))
      .map(_._1._2.map(v => v._1._1.get -> v._2)).map(l => JObject(Map(l:_*)))

    jsonNull | jsonBool | jsonNumber | jsonString | jsonArray | jsonObject
  }
}
