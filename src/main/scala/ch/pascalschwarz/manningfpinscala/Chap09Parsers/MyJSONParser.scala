package ch.pascalschwarz.manningfpinscala.Chap09Parsers

import ch.pascalschwarz.manningfpinscala.Chap09Parsers.JSON._

// ex 09_09
object MyJSONParser {
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val jsonNull: Parser[JNull.type] = "null".token.slice.map(_ => JNull)

    // TODO: NaN, Infinity
    val jsonNumber: Parser[JNumber] = ("-?[0-9]+\\.?[0-9]*".r | "-?\\.[0-9]+".r).token map (d => JNumber(d.toDouble))

    // we might want to handle toBoolean error and result in parse-fail... but it should not fail here
    val jsonBool: Parser[JBool] = ("true" | "false").token map (b => JBool(b.toBoolean)) label "boolean"


    object stringParsing {
      val unicode:Parser[String] = ("\\u" *> "[0-9a-fA-F]{4}".r) map {u =>
        Character.toChars(Integer.parseInt(u, 16)).head.toString
      } label "illegal unicode"
      val escapedChar:Parser[String] = (char('\\') *> ".".r) map (_.substring(0,1))
      val regularString:Parser[String] = "[^\"\\\\]+".r
    }
    val jsonString: Parser[JString] = surround("\"", "\"") {
      import stringParsing._
      (attempt(unicode) | escapedChar | regularString).many
    }.token.map(s => JString(s.mkString)) scope "string"

    val lit: Parser[JSON] = jsonNull | jsonBool | jsonNumber | jsonString

    def jsonArray: Parser[JArray] = surround("[", "]") {
      value.many(",".token)
    }.token.map(l => JArray(l.to[IndexedSeq])) scope "array"

    def jsonObject: Parser[JObject] = surround("{".token, "}".token) {
      ((jsonString <* ":".token) ** value).many(",".token)
    }.token.map(_.map(e => e._1.get -> e._2)).map(l => JObject(l.toMap)) scope "object"

    def value = lit | jsonArray | jsonObject
    root(value) scope "json"
  }
}
