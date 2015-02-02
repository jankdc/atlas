package com.jankdc.atlas

import tokens._
import collection.mutable.Buffer

object lexer {
  def lex(source: String): Seq[Token] = {
    val pipeline =
      makeTokens  _ andThen
      makeIndents _ andThen
      removeWhitespace _

    pipeline(source)
  }

  // Creates all the tokens that it could find
  // (using the longest match rule).
  private
  def makeTokens(source: String): Seq[Token] = {
    var tokens = Buffer[Token]()
    var buffer = source

    // This is the starting source position of any text.
    // By default, it is at (1,1). May decide to make this
    // more configurable.
    var (row, column) = (1, 1)

    // Keep consuming the source text until one has
    // found all the possible tokens within the text.
    while (! buffer.isEmpty) {
      val (group, raw) = findLongestMatch(buffer)
      val token = Token(group, raw, Line(row, column))

      // Reset the source position count to beginning of
      // a new line.
      if (group == NewLine) {
        row += 1
        column = 1
      }
      else {
        column += raw.length
      }

      tokens += token
      buffer = buffer.substring(raw.length)
    }

    tokens.toSeq
  }

  // Generates indentation based on the amount of spaces
  // on the current line. See Python indentation rules
  // for more information:
  // https://docs.python.org/release/2.5.1/ref/indentation.html
  private
  def makeIndents(tokens: Seq[Token]): Seq[Token] = {
    if (tokens.isEmpty) {
      return Seq()
    }

    var buffer = Buffer[Token]()
    var levels = Buffer[Int](0)

    for (ts <- tokens.split(NewLine)) {
      val spaces = ts.takeWhile(t => t.group == Space)

      spaces.length match {
        case x if x > levels.head =>
          val space = spaces.head
          levels +:= x
          buffer :+= Token(Indent, " " * x, space.line)
        case x if x < levels.head =>
          val ln = ts.head.line
          val dents = rollover(x, ln, levels)
          buffer ++= dents
          levels.remove(0, dents.length)
        case _ =>
          // Do nothing
      }

      buffer ++= ts
    }

    val Token(_, raw, ln) = tokens.last
    buffer ++= rollover(0, Line(ln.row,ln.column+raw.length), levels)
    buffer.toSeq
  }

  private
  def rollover(x: Int, ln: Line, l: Buffer[Int]): Seq[Token] = {
    var buffer = Buffer[Token]()
    var levels = l.clone()
    while (x < levels.head) {
      val oldH = levels.remove(0)
      val newH = levels.head
      if (x > newH)
        buffer +:= Token(Badent, " " * x, ln)
      else
        buffer +:= Token(Dedent, " " * x, ln)
    }
    buffer.toSeq
  }

  private
  def removeWhitespace(tokens: Seq[Token]): Seq[Token] =
    tokens.filterNot(t => t.group == Space || t.group == Comment)

  private
  def findLongestMatch(src: String): (Group, String) =
    tokens.groups.foldLeft((Unknown.asInstanceOf[Group], "")) {
      case ((lhs, raw), (rhs: Group)) =>
        val matched = rhs.pattern.findPrefixOf(src).mkString
        matched.length > raw.length match {
          case true => (rhs, matched)
          case false=> (lhs, raw)
        }
    }

  private
  implicit class RichTokens(val ts: Seq[Token]) extends AnyVal {
    // Split the list of tokens via a token delimiter.
    // Mainly used to divide the list of tokens via NewLine.
    def split(by: Group): Seq[Seq[Token]] = {
      var lines = Buffer[Seq[Token]]()
      var buffer = ts
      while (! buffer.isEmpty) buffer.span(t => t.group != by) match {
        case (Seq(), Seq()) =>
          // Do nothing
        case (line, Seq()) =>
          lines.append(line)
          buffer = Seq()
        case (line, delim+:rest) =>
          lines.append(line:+delim)
          buffer = rest
      }
      lines.toSeq
    }
  }
}
