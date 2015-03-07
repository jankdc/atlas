package atlas

import tokens.Token
import patterns.Pattern
import collection.mutable.{ Buffer, Stack }

object lex {

  def apply(s: String) =
    ((mkTokens _) andThen
     (_.mkIndent) andThen
     (_.filterNot(_.isInstanceOf[tokens.Spaces])) andThen
     (_.filterNot(_.isInstanceOf[tokens.Comment])))(s)

  private def mkTokens(s: String): Seq[Token] = {
    var buffer = Buffer[Token]()
    var source = s

    implicit var pos = SourcePos(1, 1)

    while (! source.isEmpty) {
      val token = findLongest(source, pos)

      pos = token match {
        case _: tokens.NewLine | _: tokens.Comment =>
          pos.copy(row = pos.row + 1, column = 1)
        case _ =>
          pos.copy(column = pos.column + token.raw.length)
      }

      buffer += token
      source = source.substring(token.raw.length)
    }

    buffer += tokens.EOF()
    buffer.toSeq
  }

  private def findLongest(s: String, pos: SourcePos): Token = {
    Pattern.all.foldLeft((patterns.Unknown.create("", pos))) {
      case (token, pattern) =>
        val matched = pattern.search(s)
        if (matched.length > token.raw.length)
          pattern.create(matched, pos)
        else
          token
    }
  }

  private implicit class TokenOps(val ts: Seq[Token]) extends AnyVal {

    def mkIndent: Seq[Token] = {
      if (ts.isEmpty) return Seq()

      var indent = Stack[Int](0)
      var buffer = Buffer[Token]()

      for (line <- ts.rmEmptyLines.lines) {
        implicit val pos = line.head.pos
        implicit val num = line.head match {
          case tokens.Spaces(n) => n.length
          case otherwise => 0
        }

        def hasMoreIndent = num > indent.top
        def hasLessIndent = num < indent.top

        if (hasMoreIndent) {
          indent.push(num)
          buffer += tokens.Indent()
        }

        while (hasLessIndent) {
          indent.pop()
          if (num > indent.top)
            buffer += tokens.Badent()
          else
            buffer += tokens.Dedent()
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
      var buffer = Buffer[Seq[Token]]()
      var remain = ts

      while (! remain.isEmpty)
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

    def rmEmptyLines: Seq[Token] =
      ts
       .lines
       .filterNot(_.head.isInstanceOf[tokens.NewLine])
       .flatten
  }

}
