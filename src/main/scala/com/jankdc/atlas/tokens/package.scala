package com.jankdc.atlas

package object tokens {
  // Used for finding the longest matched string against an RE.
  // Top-most has higher priority.
  val groups = Seq[Group](
    Fn,
    Let,
    Mut,
    Name,
    Number,
    ParenL,
    ParenR,
    Comma,
    Assign,
    Colon,
    Space,
    NewLine,
    Comment,
    Unknown)
}
