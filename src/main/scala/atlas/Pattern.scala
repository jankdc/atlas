package atlas
package patterns

import tokens.Token
import util.matching.Regex

sealed trait Pattern extends Product {
  def r: Regex
  def create(s: String, p: SourcePos): Token
  def search(s: String): String = (r findPrefixOf s).mkString
}

case object Reserved extends Pattern {
  val r: Regex = Pattern.reserved
  def create(s: String, p: SourcePos): Token = tokens.Reserved(s)(p)
}

case object NewLine extends Pattern {
  val r: Regex = Pattern.newline
  def create(s: String, p: SourcePos): Token = tokens.NewLine(s)(p)
}

case object Unknown extends Pattern {
  val r: Regex = Pattern.unknown
  def create(s: String, p: SourcePos): Token = tokens.Unknown(s)(p)
}

case object Comment extends Pattern {
  val r: Regex = Pattern.comment
  def create(s: String, p: SourcePos): Token = tokens.Comment(s)(p)
}

case object Integer extends Pattern {
  val r: Regex = Pattern.integer
  def create(s: String, p: SourcePos): Token = tokens.Integer(s)(p)
}

case object NameId extends Pattern {
  val r: Regex = Pattern.nameId
  def create(s: String, p: SourcePos): Token = tokens.NameId(s)(p)
}

case object Spaces extends Pattern {
  val r: Regex = Pattern.spaces
  def create(s: String, p: SourcePos): Token = tokens.Spaces(s)(p)
}

case object Indent extends Pattern {
  val r: Regex = Pattern.spaces
  def create(s: String, p: SourcePos): Token = tokens.Indent(s)(p)
}

case object Dedent extends Pattern {
  val r: Regex = Pattern.spaces
  def create(s: String, p: SourcePos): Token = tokens.Dedent(s)(p)
}

case object Badent extends Pattern {
  val r: Regex = Pattern.spaces
  def create(s: String, p: SourcePos): Token = tokens.Badent(s)(p)
}

case object EOF extends Pattern {
  val r: Regex = Pattern.spaces
  def create(s: String, p: SourcePos): Token = tokens.EOF(s)(p)
}

object Pattern {
  import collection.mutable.Buffer

  lazy val all = {
    var buffer = Buffer[Pattern]()
    buffer += Reserved
    buffer += NameId
    buffer += Integer
    buffer += NewLine
    buffer += Spaces
    buffer += Unknown
    buffer += Comment
    buffer.toSeq
  }

  lazy val reserved = {
    var buffer = Buffer[String]()
    buffer += "(static)"
    buffer += "(\\()"
    buffer += "(\\))"
    buffer += "(pass)"
    buffer += "(mut)"
    buffer += "(let)"
    buffer += "(fn)"
    buffer += "(as)"
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

  lazy val nameId = "[a-zA-Z]\\w*".r
  lazy val spaces = " *".r
  lazy val integer = "(0)|([1-9][0-9]*)".r
  lazy val comment = "(#.*\\n)|(#.*\\r\\n)".r
  lazy val newline = "(\\n)|(\\r\\n)".r
  lazy val unknown = "((?s).)".r
}
