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
        case _: tokens.NewLine |
             _: tokens.Comment =>
          pos.copy(row = pos.row + 1, col = 1)
        case _ =>
          pos.copy(col = pos.col + token.raw.length)
      }

      buffer += token
      source = source.substring(token.raw.length)
    }

    buffer += tokens.EOF()(pos)
    buffer.toSeq
          .mkIndent
          .filterNot(_.isInstanceOf[tokens.WhiteSp])
          .filterNot(_.isInstanceOf[tokens.Comment])
  }

  private def findLongest(s: String, p: LinePos): Token =
    patterns.foldLeft(tokens.Unknown("")(p): Token) {
      case (token, (regex, tokenGen)) =>
        val matched = regex.findPrefixOf(s).mkString
        if (token.raw.length >= matched.length)
          token
        else
          tokenGen(matched, p)
    }

  private implicit class TokenOps(val ts: Seq[Token]) {
    def mkIndent: Seq[Token] = {
      if (ts.isEmpty) return Seq()

      val indent = mutable.Stack[Int](0)
      val buffer = mutable.Buffer[Token]()

      for (line <- ts.nonEmptyLines) {
        val pos = line.head.pos
        val num = line.head match {
          case tokens.WhiteSp(n) => n.length
          case otherwise => 0
        }

        def hasMoreIndent = num > indent.top
        def hasLessIndent = num < indent.top

        if (hasMoreIndent) {
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

    def lines: Seq[Seq[Token]] = {
      val buffer = mutable.Buffer[Seq[Token]]()
      var remain = ts

      while (remain.nonEmpty)
        remain.span(!_.isInstanceOf[tokens.NewLine]) match {
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
      ts.lines.filter(!_.head.isInstanceOf[tokens.NewLine])
  }

  private type TokenGen = (String, LinePos) => Token
  private type Pattern  = (Regex, TokenGen)

  private lazy val patterns: Seq[Pattern] = Seq(
    (reserve, (s: String, p: LinePos) => tokens.Reserve(s)(p)),
    (namedId, (s: String, p: LinePos) => tokens.NamedId(s)(p)),
    (integer, (s: String, p: LinePos) => tokens.Integer(s)(p)),
    (whiteSp, (s: String, p: LinePos) => tokens.WhiteSp(s)(p)),
    (newline, (s: String, p: LinePos) => tokens.NewLine(s)(p)),
    (comment, (s: String, p: LinePos) => tokens.Comment(s)(p)),
    (unknown, (s: String, p: LinePos) => tokens.Unknown(s)(p)))

  private lazy val reserve = {
    val buffer = mutable.Buffer[String]()
    buffer += "(static)"
    buffer += "(pass)"
    buffer += "(mut)"
    buffer += "(let)"
    buffer += "(true)"
    buffer += "(false)"
    buffer += "(fn)"
    buffer += "(as)"
    buffer += "(\\()"
    buffer += "(\\))"
    buffer += "(->)"
    buffer += "(,)"
    buffer += "(:)"
    buffer += "(==)"
    buffer += "(!=)"
    buffer += "(<=)"
    buffer += "(=)"
    buffer += "(>=)"
    buffer += "(<)"
    buffer += "(>)"
    buffer += "(_)"
    buffer += "([+-/*])"
    buffer.mkString("|").r
  }

  private lazy val comment = "(#.*\\n)|(#.*\\r\\n)".r
  private lazy val integer = "(0)|([1-9][0-9]*)".r
  private lazy val newline = "(\\n)|(\\r\\n)".r
  private lazy val namedId = "[a-zA-Z]\\w*".r
  private lazy val unknown = "((?s).)".r
  private lazy val whiteSp = " *".r

}
