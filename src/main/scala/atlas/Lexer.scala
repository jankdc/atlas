package atlas

import atlas.tokens.Token
import scala.collection.mutable
import scala.util.matching.Regex

object Lexer {
  def mkTokens(s: String): Seq[Token] = {
    val buffer = mutable.Buffer[Token]()
    var source = s
    var pos = LinePos(1, 1)

    while (! source.isEmpty) {
      val token = findLongest(source, pos)

      pos = token match {
        case _: tokens.Newline |
             _: tokens.Comment =>
          pos.copy(row = pos.row + 1, col = 1)
        case _ =>
          pos.copy(col = pos.col + token.raw.length)
      }

      buffer += token
      source = source.substring(token.raw.length)
    }

    import atlas.TokenSeqOps._

    val genTokens = buffer
     .toSeq
     .map { case t: tokens.Comment => tokens.Newline("\n")(t.pos); case t => t }
     .mkIndent
     .filterNot(_.isInstanceOf[tokens.Whitespace])

    genTokens :+ tokens.EOF()(pos)
  }

  private def findLongest(s: String, p: LinePos): Token = {
    patterns.foldLeft(tokens.Unknown("")(p): Token) {
      case (token, (regex, tokenGen)) =>
        val matched = regex.findPrefixOf(s).mkString
        if (token.raw.length >= matched.length)
          token
        else
          tokenGen(matched, p)
    }
  }

  private type TokenGen = (String, LinePos) => Token
  private type Pattern  = (Regex, TokenGen)

  private lazy val patterns: Seq[Pattern] = Seq(
    (reserve, (s: String, p: LinePos) => tokens.Reserve(s)(p)),
    (boolean, (s: String, p: LinePos) => tokens.Boolean(s)(p)),
    (identifier, (s: String, p: LinePos) => tokens.Identifier(s)(p)),
    (integer, (s: String, p: LinePos) => tokens.Integer(s)(p)),
    (whitespace, (s: String, p: LinePos) => tokens.Whitespace(s)(p)),
    (newline, (s: String, p: LinePos) => tokens.Newline(s)(p)),
    (comment, (s: String, p: LinePos) => tokens.Comment(s)(p)),
    (unknown, (s: String, p: LinePos) => tokens.Unknown(s)(p)))

  private lazy val reserve = {
    val buffer = mutable.Buffer[String]()
    buffer += "(static)"
    buffer += "(pass)"
    buffer += "(mut)"
    buffer += "(let)"
    buffer += "(while)"
    buffer += "(for)"
    buffer += "(to)"
    buffer += "(fn)"
    buffer += "(if)"
    buffer += "(else)"
    buffer += "(elif)"
    buffer += "(or)"
    buffer += "(and)"
    buffer += "(\\()"
    buffer += "(\\))"
    buffer += "(\\[)"
    buffer += "(\\])"
    buffer += "(->)"
    buffer += "(,)"
    buffer += "(:)"
    buffer += "(==)"
    buffer += "(!=)"
    buffer += "(<=)"
    buffer += "(=)"
    buffer += "(>=)"
    buffer += "(\\+=)"
    buffer += "(<)"
    buffer += "(%)"
    buffer += "(>)"
    buffer += "(!)"
    buffer += "([+-/*])"
    buffer.mkString("|").r
  }

  private lazy val boolean = "(true|false)".r
  private lazy val comment = "( *#.*\\n)|( *#.*\\r\\n)".r
  private lazy val integer = "(0)|([1-9][0-9]*)".r
  private lazy val newline = "(\\n)|(\\r\\n)".r
  private lazy val identifier = "[a-zA-Z]\\w*".r
  private lazy val unknown = "((?s).)".r
  private lazy val whitespace = " *".r

}
