package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

case class Context(typeset: Set[String], bindings: Map[Sym, Seq[Type]]) {
  def addVar(s: Sym, t: Type): Context = {
    checkType(s.pos, t)

    bindings.get(s) match {
      case None =>
        this.copy(bindings = this.bindings + (s -> Seq(t)))
      case some =>
        throw CheckError(s"${s.pos}: ${s.name} is already defined.")
    }
  }

  def getVar(s: Sym): Type =
    bindings
      .toSeq
      .collect {
        case (Sym(_, sm), ts) if sm == s.name => ts.collectFirst {
          case t: types.Var => t
          case t: types.Fun if t.terms.length == 1 => t
        }
      }
      .flatten
      .headOption
      .getOrElse(throw CheckError(s"${s.pos}: ${s.name} is undefined."))

  def addFun(s: Sym, t: Type): Context = {
    checkType(s.pos, t)

    bindings.get(s) match {
      case None =>
        this.copy(bindings = this.bindings + (s -> Seq(t)))
      case Some(Seq(types.Var(_))) =>
        throw CheckError(s"${s.pos}: ${s.name} is already defined.")
      case Some(ts) if ts.contains(t) =>
        throw CheckError(s"${s.pos}: ${s.name} is already defined.")
      case Some(ts) =>
        this.copy(bindings = this.bindings + (s -> (t +: ts)))
    }
  }

  def getFun(s: Sym, as: Seq[Type]): (String, Type) =
    bindings
      .toSeq
      .collect {
        case (Sym(sc, sm), ts) if sm == s.name => ts.collectFirst {
          case t@types.Fun(ps :+ rt) if ps == as => (sc, rt)
        }
      }
      .flatten
      .headOption
      .getOrElse(throw CheckError(s"${s.pos}: ${s.name} is undefined."))

  def addType(s: String): Context =
    this.copy(typeset = this.typeset + s)

  private def checkType(p: LinePos, t: Type): Unit = t match {
    case types.Fun(terms) => terms.foreach(checkType(p, _))
    case types.App(_, ts) => checkType(p, t)
    case types.Var(tname) =>
      if (! typeset.contains(tname))
        throw CheckError(s"$p: Type is not found: $tname")
  }
}

object Context {
  def apply(): Context = Context(Set(), Map())
}