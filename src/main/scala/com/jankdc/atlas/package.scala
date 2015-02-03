package com.jankdc

package object atlas {
  case class Line(row: Int, column: Int)
  case class Node(group: nodes.Group, line: Line)
  case class Type(group: types.Group, line: Line)
  case class Token(group: tokens.Group, raw: String, line: Line)
}
