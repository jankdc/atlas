package atlas

import token._
import scala.collection.mutable.Buffer
import scala.io.Source

object Lexer {
  def main(args: Array[String]): Unit = {
    val stream = Source.fromURL(getClass.getResource("/main.atlas"))
    val source = stream.mkString
    val tokens = fromSource(source)

    tokens.foreach(println)

    stream.close()
  }

  def fromSource(src: String): Seq[Token] = {
    var tokens = generateTokens(src)
    tokens = generateLayout(tokens)
    tokens = tokens.sortBy(t => (t.row, t.column))
    tokens = tokens.filterNot(t => t.group == Space)
    tokens = tokens.filterNot(t => t.group == Comment)
    tokens
  }

  private
  def generateTokens(src: String): Seq[Token] = {
    var tokens = Buffer[Token]()
    var buffer = src
    var column = 1
    var row = 1
    while (! buffer.isEmpty) {
      val (group, raw) = findLongestMatch(buffer)
      tokens += Token(group, raw, row, column)
      column += raw.length
      if (group == NewLine) {
        row = row + 1
        column = 1
      }
      buffer = buffer.substring(raw.length)
    }

    tokens.toSeq
  }

  private
  def generateLayout(tokens: Seq[Token]): Seq[Token] = {
    if (tokens.isEmpty) {
      return Seq()
    }
    var newTokens = Buffer[Token]()
    var level = Buffer[Int](0)
    for (line <- splitBy(tokens, NewLine)) {
      val sps = line.takeWhile(t => t.group == Space)
      sps.length match {
        case x if x > level.head =>
          val sp = sps.head
          level +:= x
          newTokens :+= Token(Indent, " " * x, sp.row, sp.column)
        case x if x < level.head =>
          val newds = rollover(x, line.head.row, level)
          newTokens ++= newds
          level.remove(0, newds.length)
        case _ =>
          // Do nothing
      }
      newTokens ++= line
    }
    newTokens ++= rollover(0, tokens.last.row + 1, level)
    newTokens.toSeq
  }

  private
  def rollover(x: Int, row: Int, lv: Buffer[Int]): Seq[Token] = {
    var dents = Buffer[Token]()
    var level = lv.clone()
    while (x < level.head) {
      val oldH = level.remove(0)
      val newH = level.head
      if (x > newH) {
        dents +:= Token(Badent, " " * x, row, 1)
      }
      else {
        dents +:= Token(Dedent, " " * x, row, 1)
      }
    }
    dents.toSeq
  }

  private
  def splitBy(tokens: Seq[Token], by: Group): Seq[Seq[Token]] = {
    var lines = Buffer[Seq[Token]]()
    var buffer = tokens
    while (! buffer.isEmpty) {
      buffer.span(t => t.group != by) match {
        case (Seq(), Seq()) =>
          // Do nothing
        case (line, Seq()) =>
          lines.append(line)
          buffer = Seq()
        case (line, delim+:rest) =>
          lines.append(line:+ delim)
          buffer = rest
      }
    }
    lines.toSeq
  }

  private
  def findLongestMatch(src: String): (Group, String) = {
    token.groups.foldLeft((Unknown.asInstanceOf[Group], "")) {
      case ((group, raw), (gp: Group)) =>
        val matched = gp.re.findPrefixOf(src).mkString
        matched.length > raw.length match {
          case true => (gp, matched)
          case false => (group, raw)
        }
    }
  }
}
