package com.jankdc.atlas.nodes

import com.jankdc.atlas.Node

sealed trait Group
case class Top(nodes: Seq[Node]) extends Group
case class Abs(name: Node, params: Seq[Node], body: Seq[Node]) extends Group
case class App(name: Node, args: Seq[Node]) extends Group
case class Let(name: Node, rval: Node) extends Group
case class Mut(name: Node, rval: Node) extends Group
case class Param(lhs: Node, rhs: Node) extends Group
case class Name(value: String) extends Group
case class Number(value: Int) extends Group
case class Type(value: Node) extends Group
case class Nop() extends Group
