package atlas
package tokens

sealed trait Token extends Product {
  def raw: String
  def pos: LinePos
}

case class Whitespace(raw: String)(val pos: LinePos) extends Token
case class Identifier(raw: String)(val pos: LinePos) extends Token
case class Reserved(raw: String)(val pos: LinePos) extends Token
case class Newline(raw: String)(val pos: LinePos) extends Token
case class Unknown(raw: String)(val pos: LinePos) extends Token
case class Comment(raw: String)(val pos: LinePos) extends Token
case class Integer(raw: String)(val pos: LinePos) extends Token
case class Boolean(raw: String)(val pos: LinePos) extends Token
case class Indent(raw: String = "indent")(val pos: LinePos) extends Token
case class Dedent(raw: String = "dedent")(val pos: LinePos) extends Token
case class Badent(raw: String = "badent")(val pos: LinePos) extends Token
case class EOF(raw: String = "EOF")(val pos: LinePos) extends Token
