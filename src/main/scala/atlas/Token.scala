package atlas
package tokens

sealed trait Token extends Product with Location {
  def raw: String
}

case class Reserved(val raw: String)
 (implicit val pos: SourcePos) extends Token

case class NewLine(val raw: String)
 (implicit val pos: SourcePos) extends Token

case class Unknown(val raw: String)
 (implicit val pos: SourcePos) extends Token

case class Comment(val raw: String)
 (implicit val pos: SourcePos) extends Token

case class Integer(val raw: String)
 (implicit val pos: SourcePos) extends Token

case class NameId(val raw: String)
 (implicit val pos: SourcePos) extends Token

case class Spaces(val raw: String)
 (implicit val pos: SourcePos) extends Token

// The tokens below are currently generated by the lexer.
// This is why, as default, their lexemes are empty.
// However, future design considerations may allow these tokens
// to accept other lexemes (e.g. brackets for Indentations).

case class Indent(val raw: String = "")
 (implicit val pos: SourcePos) extends Token

case class Badent(val raw: String = "")
 (implicit val pos: SourcePos) extends Token

case class Dedent(val raw: String = "")
 (implicit val pos: SourcePos) extends Token

case class EOF(val raw: String = "")
 (implicit val pos: SourcePos) extends Token
