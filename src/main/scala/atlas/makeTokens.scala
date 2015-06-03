package atlas

import atlas.tokens._
import scala.collection.mutable
import scala.util.matching.Regex

object makeTokens {
  def apply(s: String): Seq[Token] = {
    val buffer = mutable.Buffer[Token]()
    var source = s
    var pos = LinePos(1, 1)

    while (source.nonEmpty) {
      val token = findLongest(source, pos)

      pos = token match {
        case _: Newline | _: Comment =>
          pos.copy(row = pos.row + 1, col = 1)
        case _ =>
          pos.copy(col = pos.col + token.raw.length)
      }

      buffer += token
      source = source.substring(token.raw.length)
    }

    buffer += EOF()(pos)

    buffer
     .toSeq
     .removeNewlines
     .makeIndents
     .removeWhitespaces
  }

  private def findLongest(s: String, p: LinePos): Token =
    patterns.foldLeft(Unknown("")(p): Token) {
      case (token, (regex, tokenGen)) =>
        val matched = regex.findPrefixOf(s).mkString
        if (token.raw.length >= matched.length)
          token
        else
          tokenGen(matched, p)
    }

  private type TokenGen = (String, LinePos) => Token
  private type Pattern  = (Regex, TokenGen)

  private lazy val patterns = Seq(
    (reserved,     (s: String, p: LinePos) => Reserved(s)(p)),
    (boolean,      (s: String, p: LinePos) => Boolean(s)(p)),
    (identifier,   (s: String, p: LinePos) => Identifier(s)(p)),
    (integer,      (s: String, p: LinePos) => Integer(s)(p)),
    (whitespace,   (s: String, p: LinePos) => Whitespace(s)(p)),
    (newline,      (s: String, p: LinePos) => Newline(s)(p)),
    (comment,      (s: String, p: LinePos) => Comment(s)(p)),
    (unknown,      (s: String, p: LinePos) => Unknown(s)(p)))

  private lazy val reserved = Seq(
    "(static)",
    "(pass)",
    "(mut)",
    "(let)",
    "(while)",
    "(for)",
    "(to)",
    "(fn)",
    "(if)",
    "(else)",
    "(elif)",
    "(or)",
    "(and)",
    "(\\()",
    "(\\))",
    "(\\[)",
    "(\\])",
    "(,)",
    "(=)",
    "(:)",
    "(==)",
    "(!=)",
    "(<=)",
    "(>=)",
    "(\\+=)",
    "(\\+)",
    "(-)",
    "(/)",
    "(\\*)",
    "(=)",
    "(<)",
    "(>)",
    "(%)",
    "(!)"
  ).mkString("|").r

  private lazy val boolean = "(true|false)".r
  private lazy val comment = "( *#.*\\n)|( *#.*\\r\\n)".r
  private lazy val integer = "(0)|([1-9][0-9]*)".r
  private lazy val newline = "(\\n)|(\\r\\n)".r
  private lazy val unknown = "((?s).)".r
  private lazy val identifier = "[a-zA-Z]\\w*".r
  private lazy val whitespace = " *".r

  private implicit class TokenSeqOps(val ts: Seq[Token]) extends AnyVal {
    def makeIndents: Seq[Token] = {
      if (ts.isEmpty) return Seq()

      val indent = mutable.Stack[Int](0)
      val buffer = mutable.Buffer[Token]()

      for (line <- ts.nonEmptyLines) {
        val pos = line.head.pos
        val num = line.head match {
          case tokens.Whitespace(n) => n.length
          case otherwise => 0
        }

        def hasMoreIndent() = num > indent.top
        def hasLessIndent() = num < indent.top

        if (hasMoreIndent()) {
          indent.push(num)
          buffer += tokens.Indent()(pos)
        }

        while (hasLessIndent) {
          indent.pop()
          if (num > indent.top)
            buffer += tokens.Badent()(pos)
          else
            buffer += tokens.Dedent()(pos)
        }

        buffer ++= line
      }

      while (0 < indent.top) {
        indent.pop()
        buffer += tokens.Dedent()(ts.last.pos)
      }

      buffer.toSeq
    }

    def removeNewlines: Seq[Token] =
      ts.map { case t: Comment => Newline("\n")(t.pos); case t => t }

    def removeWhitespaces: Seq[Token] =
      ts.filterNot(_.isInstanceOf[Whitespace])

    def lines: Seq[Seq[Token]] = {
      val buffer = mutable.Buffer[Seq[Token]]()
      var remain = ts

      while (remain.nonEmpty)
        remain.span(!_.isInstanceOf[tokens.Newline]) match {
          case (Seq(),Seq()) =>
            return buffer.toSeq
          case (line, Seq()) =>
            buffer.append(line)
            remain = Seq()
          case (line, delim+:rest) =>
            buffer.append(line:+delim)
            remain = rest
        }

      buffer.toSeq
    }

    def nonEmptyLines: Seq[Seq[Token]] =
      ts.lines.filter(!_.head.isInstanceOf[tokens.Newline])
  }
}