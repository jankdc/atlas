package atlas.token

import util.matching.Regex

case class Token(group: Group, raw: String, row: Int, column: Int)

sealed abstract class Group(val re: Regex)
case object Name    extends Group("[a-zA-Z]\\w*".r)
case object Number  extends Group("(0)|([1-9][0-9]*)".r)
case object Func    extends Group("fn".r)
case object Let     extends Group("let".r)
case object Mut     extends Group("mut".r)
case object ParenL  extends Group("\\(".r)
case object ParenR  extends Group("\\)".r)
case object Comma   extends Group(",".r)
case object Colon   extends Group(":".r)
case object Space   extends Group(" ".r)
case object NewLine extends Group("(\\n)|(\\r\\n)".r)
case object Comment extends Group("(#.*\\n)|(#.*\\r\\n)".r)
case object Unknown extends Group("(?s).".r)

// Generated tokens. Do not put in values.
case object Indent  extends Group("".r)
case object Dedent  extends Group("".r)
case object Badent  extends Group("".r)
