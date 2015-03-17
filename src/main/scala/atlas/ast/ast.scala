package atlas
package ast

sealed trait Node {
  def pos: LinePos
}

case class Top(nodes: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = nodes.mkString("\n"*2)
}

case class Let(name: String, value: Node)
  (val pos: LinePos) extends Node with Bound {
  override def toString = s"let $name = $value"
}

case class Sig(terms: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = terms.mkString(" -> ")
}

case class Mut(name: String, value: Node)
  (val pos: LinePos) extends Node with Bound {
  override def toString = s"let mut $name = $value"
}

case class Nop()
  (val pos: LinePos) extends Node {
  override def toString = s"pass"
}

case class App(name: String, args: Seq[Node])
  (val pos: LinePos) extends Node with Bound {
  override def toString = s"$name(${args.mkString(", ")})"
}

case class Fun(name: String, terms: Seq[Node], body: Seq[Node])
  (val pos: LinePos) extends Node with Bound {
  override def toString = {
    val indent = "\n" + (" " * (2 + pos.col))
    val mainNd = body.mkString(s"$indent")
    val params = terms.init.mkString(", ")
    val retval = terms.last

    s"fn $name($params): $retval$indent$mainNd"
  }
}

case class UnaOp(op: String, rhs: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"($op $rhs)"
}

case class BinOp(lhs: Node, op: String, rhs: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"($lhs $op $rhs)"
}

case class Operator(value: String)
  (val pos: LinePos) extends Node {
  override def toString = value
}

case class List(nodes: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = nodes.mkString(", ")
}

case class Param(name: String, typename: Node)
  (val pos: LinePos) extends Node with Bound {
  override def toString = s"$name: $typename"
}

case class NamedId(name: String)
  (val pos: LinePos) extends Node with Bound {
  override def toString = name
}

case class Static(name: String, typename: Node, value: Node)
  (val pos: LinePos) extends Node with Bound {
  override def toString = s"static $name: $typename = $value"
}

case class Integer(value: Int)
  (val pos: LinePos) extends Node {
  override def toString = value.toString
}

