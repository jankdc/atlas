package atlas

import nodes.Node
import types.Type

case class NodeLink(p: Node, n: Node)

object check {
  def apply(c: Context, n: NodeLink): (Context, Type) = n match {
    case NodeLink(p, nodes.Top(ns)) =>
      val cc = ns foldLeft(c) {
        case (c, n: nodes.Static) =>
          (c.addVar(n, c.mkType(n.typename)), types.Var("Unit"))
        case (c, n: nodes.Function) =>
          val ps = n.terms map {
            case n: nodes.Param => c.mkType(n.typename)
            case n => c.mkType(n)
          }
          (c.addAbs(n, types.Abs(ps)), types.Var("Unit"))
      }

      for (n <- ns) {
        val t = this(cc, NodeLink(n.n, n))
        if (t != types.Var("Unit")) {
          throw CheckError("[${n.pos}]: Expected Unit, got $t")
        }
      }

      (cc, types.Var("Unit"))

    case NodeLink(p, nodes.Static(s, t, v)) =>
      ???
    case _ =>
      ???
  }
}