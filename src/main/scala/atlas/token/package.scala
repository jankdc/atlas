package atlas

package object token {
  // Used for finding the longest matched string against an RE.
  // Top-most has higher priority.
  val groups = Seq[Group](
    Func,
    Let,
    Mut,
    Name,
    Number,
    ParenL,
    ParenR,
    Comma,
    Colon,
    Space,
    NewLine,
    Comment,
    Unknown)
}
