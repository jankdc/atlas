package atlas.nodes

import atlas.{ SourcePos, Location, Bound }

sealed trait Node extends Location

case class Top(nodes: Seq[Node])
 (implicit val pos: SourcePos) extends Node {
  override def toString = nodes.mkString("\n"*2)
}

case class Let(val name: String, value: Node)
 (implicit val pos: SourcePos) extends Node with Bound {
  override def toString = s"let $name = $value"
}

case class Mut(val name: String, value: Node)
 (implicit val pos: SourcePos) extends Node with Bound {
  override def toString = s"let mut $name = $value"
}

case class Nop()
 (implicit val pos: SourcePos) extends Node {
  override def toString = s"pass"
}

case class App(val name: String, args: Seq[Node])
 (implicit val pos: SourcePos) extends Node with Bound {
  override def toString = s"$name(${args.mkString(", ")})"
}

case class Fun(val name: String, terms: Seq[Node], body: Seq[Node])
 (implicit val pos: SourcePos) extends Node with Bound {
  override def toString = {
    val indent = "\n" + (" " * (2 + pos.column))
    val mainNd = body.mkString(s"$indent")
    val params = terms.init.mkString(", ")
    val retval = terms.last

    s"fn $name($params): $retval$indent$mainNd"
  }
}

case class List(nodes: Seq[Node])
 (implicit val pos: SourcePos) extends Node {
  override def toString = nodes.mkString("\n")
}

case class Param(val name: String, typename: Node)
 (implicit val pos: SourcePos) extends Node with Bound {
  override def toString = s"$name: $typename"
}

case class NameId(val name: String)
 (implicit val pos: SourcePos) extends Node with Bound {
  override def toString = name
}

case class Integer(value: Int)
 (implicit val pos: SourcePos) extends Node {
  override def toString = value.toString
}
