package ch.pascalschwarz.manningfpinscala.Chap09Parsers

import ch.pascalschwarz.manningfpinscala.Chap09Parsers.JSON._

// ex 09_09
object MyJSONParser {
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = "\\s+".many.slice

    val jsonNull: Parser[JNull.type] = string("null").slice.map(_ => JNull)

    // TODO: NaN, Infinity
    val jsonNumber: Parser[JNumber] = (regex("-?[0-9]+\\.?[0-9]*".r) | regex("-?\\.[0-9]+".r)) map (d => JNumber(d.toDouble))

    // we might want to handle toBoolean error and result in parse-fail... but it should not fail here
    val jsonBool: Parser[JBool] = ("true" | "false") map (b => JBool(b.toBoolean))


    object stringParsing {
      val unicode:Parser[String] = (string("\\u") *> regex("[0-9a-fA-F]{4}".r)) map (u => Character.toChars(Integer.parseInt(u, 16)).head.toString)
      val escapedChar:Parser[String] = (char('\\') *> regex("."r)) map (_.substring(0,1))
      val regularString:Parser[String] = regex("[^\"\\\\]+"r)
    }
    val jsonString: Parser[JString] = surround("\"", "\"") {
      (attempt(stringParsing.unicode) | stringParsing.escapedChar | stringParsing.regularString).many
    }.map(s => JString(s.mkString))

    def lit: Parser[JSON] = jsonNull | jsonBool | jsonNumber | jsonString

    def jsonArray: Parser[JArray] = surround("[", "]") {
      lit.many(char(','))
    }.map(l => JArray(l.to[IndexedSeq]))

    def jsonObject: Parser[JObject] = surround("{", "}") {
      ((jsonString <* char(':')) ** jsonParser(P)).many(char(','))
    }.map(_.map(e => e._1.get -> e._2)).map(l => JObject(l.toMap))

    jsonNull | jsonBool | jsonNumber | jsonString | jsonArray// | jsonObject
  }
}
