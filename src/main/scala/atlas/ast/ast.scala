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
  (val pos: LinePos) extends Node {
  override def toString = s"let $name = $value"
}

case class Mut(name: String, value: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"let mut $name = $value"
}

case class Nop()(val pos: LinePos) extends Node {
  override def toString = s"pass"
}

case class App(name: String, args: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = s"$name(${args.mkString(", ")})"
}

case class Fun(name: String, params: Seq[Param], ret: Type, body: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = {
    val indent = "\n" + (" " * (1 + pos.col))
    val bdNodes = body.mkString(indent)
    val psNodes = params.mkString(", ")

    s"fn $name($psNodes): $ret$indent$bdNodes"
  }
}

case class Type(terms: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = terms.mkString(" -> ")
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
  (val pos: LinePos) extends Node {
  override def toString = s"$name: $typename"
}

case class NamedId(name: String)
  (val pos: LinePos) extends Node {
  override def toString = name
}

case class Static(name: String, typename: Node, value: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"static $name: $typename = $value"
}

case class Integer(value: Int)
  (val pos: LinePos) extends Node {
  override def toString = value.toString
}

case class Boolean(value: scala.Boolean)
  (val pos: LinePos) extends Node {
  override def toString = value.toString
}

case class Cond(cond: Node, body: Seq[Node], others: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = {
    val dent = "\n" + (" " * (1 + pos.col))
    val str1 = body.mkString(dent)
    val str2 = others.mkString("\n")
    s"if $cond$dent$str1\n$str2"
  }
}

case class Elif(cond: Node, body: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = {
    val dent = "\n" + (" " * (1 + pos.col))
    val str1 = body.mkString(dent)
    (" " * (pos.col - 1)) + s"elif $cond$dent$str1"
  }
}

case class Else(body: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = {
    val dent = "\n" + (" " * (1 + pos.col))
    val str1 = body.mkString(dent)
    (" " * (pos.col - 1)) + s"else$dent$str1"
  }
}

case class Subscript(name: String, arg: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"$name[$arg]"
}

case class Cons(typeid: Node, args: Seq[Node])
  (val pos: LinePos) extends Node {
  override def toString = s"[$typeid](${args.mkString(", ")})"
}


case class ListType(tp: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"[$tp]"
}

case class Assign(name: String, value: Node)
  (val pos: LinePos) extends Node {
  override def toString = s"mut $name = $value"
}
