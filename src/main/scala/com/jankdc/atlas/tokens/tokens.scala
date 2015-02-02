package com.jankdc.atlas.tokens

import util.matching.Regex

sealed abstract class Group(val pattern: Regex)
case object Name    extends Group("[a-zA-Z]\\w*".r)
case object Number  extends Group("(0)|([1-9][0-9]*)".r)
case object Fn      extends Group("fn".r)
case object Let     extends Group("let".r)
case object Mut     extends Group("mut".r)
case object ParenL  extends Group("\\(".r)
case object ParenR  extends Group("\\)".r)
case object Assign  extends Group("=".r)
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
case object EOF     extends Group("".r)
