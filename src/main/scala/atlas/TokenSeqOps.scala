package atlas

import atlas.tokens.Token
import scala.collection.mutable

object TokenSeqOps {
  implicit class TokenSeqOps(val ts: Seq[Token]) {
    def mkIndent: Seq[Token] = {
      if (ts.isEmpty) return Seq()

      val indent = mutable.Stack[Int](0)
      val buffer = mutable.Buffer[Token]()

      for (line <- ts.nonEmptyLines) {
        val pos = line.head.pos
        val num = line.head match {
          case tokens.Whitespace(n) => n.length
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
