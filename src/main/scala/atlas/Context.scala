package atlas

import atlas.ast.Node
import atlas.types.Type

case class Context(defTypes: Set[String], bindings: Map[Symbol, Type]) {

  def addDef(s: Symbol, t: Type): Context = {
    checkType(s.pos, t)

    bindings.get(s) match {
      case None =>
        this.copy(bindings = this.bindings + (s -> t))
      case some =>
        throw TypeError(s"${s.pos}: ${s.name} is already defined.")
    }
  }

  def getDef(s: String, p: LinePos): (Symbol, Type) =
    bindings
      .toSeq
      .find { case (Symbol(_, nm, op), _) => nm + op == s }
      .getOrElse { throw TypeError(s"$p: $s is undefined.") }

  def mkType(s: String): Context =
    this.copy(defTypes = this.defTypes + s)

  private def checkType(p: LinePos, t: Type): Unit = t match {
    case types.Fun(terms) => terms.foreach(checkType(p, _))
    case types.Var(tname) =>
      if (! defTypes.contains(tname))
        throw TypeError(s"$p: Type is not found: $tname")
  }
}

object Context { def apply(): Context = Context(Set(), Map()) }

