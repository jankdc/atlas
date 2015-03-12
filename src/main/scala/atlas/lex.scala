package atlas

import tokens._
import collection.mutable.{ Buffer, Stack }
import util.matching.Regex

object lex {
  def apply(s: String) =
    (mkTokens _  andThen
    (_.mkIndent) andThen
    (_.filterNot(_.isInstanceOf[WhiteSp])) andThen
    (_.filterNot(_.isInstanceOf[Comment])))(s)

  private def mkTokens(s: String): Seq[Token] = {
    val buffer = Buffer[Token]()
    var source = s

    implicit var pos = LinePos(1, 1)

    while (! source.isEmpty) {
      val token = findLongest(source, pos)

      pos = token match {
        case _: NewLine |
             _: Comment =>
          pos.copy(row = pos.row + 1, col = 1)
        case _ =>
          pos.copy(col = pos.col + token.raw.length)
      }

      buffer += token
      source = source.substring(token.raw.length)
    }

    buffer += EOF()
    buffer.toSeq
  }

  def findLongest(s: String, p: LinePos): Token =
    patterns.foldLeft(Unknown("")(p): Token) {
      case (token, (regex, tokenGen)) =>
        val matched = regex.findPrefixOf(s).mkString
        if (token.raw.length >= matched.length)
          token
        else
          tokenGen(matched, p)
    }

  private implicit class TokenSeqOps(val ts: Seq[Token]) extends AnyVal {
    def mkIndent: Seq[Token] = {
      if (ts.isEmpty) return Seq()

      val indent = Stack[Int](0)
      val buffer = Buffer[Token]()

      for (line <- ts.nonEmptyLines) {
        implicit val pos = line.head.pos
        implicit val num = line.head match {
          case WhiteSp(n) => n.length
          case otherwise => 0
        }

        def hasMoreIndent = num > indent.top
        def hasLessIndent = num < indent.top

        if (hasMoreIndent) {
          indent.push(num)
          buffer += Indent()
        }

        while (hasLessIndent) {
          indent.pop()
          if (num > indent.top)
            buffer += Badent()
          else
            buffer += Dedent()
        }

        buffer ++= line
      }

      while (0 < indent.top) {
        indent.pop()
        buffer += Dedent()(ts.last.pos)
      }

      buffer.toSeq
    }

    def lines: Seq[Seq[Token]] = {
      val buffer = Buffer[Seq[Token]]()
      var remain = ts

      while (remain.nonEmpty)
        remain.span(!_.isInstanceOf[NewLine]) match {
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
      ts.lines.filter(!_.head.isInstanceOf[NewLine])
  }

  private type TokenGen = (String, LinePos) => Token
  private type Pattern  = (Regex, TokenGen)

  private lazy val patterns: Seq[Pattern] = Seq(
    (reserve, (s: String, p: LinePos) => Reserve(s)(p)),
    (namedId, (s: String, p: LinePos) => NamedId(s)(p)),
    (integer, (s: String, p: LinePos) => Integer(s)(p)),
    (whiteSp, (s: String, p: LinePos) => WhiteSp(s)(p)),
    (newline, (s: String, p: LinePos) => NewLine(s)(p)),
    (comment, (s: String, p: LinePos) => Comment(s)(p)),
    (unknown, (s: String, p: LinePos) => Unknown(s)(p)))

  private lazy val reserve = {
    val buffer = Buffer[String]()
    buffer += "(static)"
    buffer += "(pass)"
    buffer += "(mut)"
    buffer += "(let)"
    buffer += "(fn)"
    buffer += "(as)"
    buffer += "(\\(\\))"
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
