package atlas

import types.Type
import nodes.Node

import ListFilter._

class Checker(outer: Context = new Context()) {
  var ctx = outer.clone()

  def check(n: Node): Type = n match {
    case n: nodes.Integer => types.Var("Int")
    case n: nodes.NameId  => ctx.getVar(n)
    case n: nodes.Let     => check(n)
    case n: nodes.Mut     => check(n)
    case n: nodes.Fun     => check(n)
    case n: nodes.Top     => check(n)
    case n: nodes.App     => check(n)
    case n: nodes.Static  => check(n)
    case others           => types.Var("Unit")
  }

  private def check(n: nodes.Let): Type = {
    ctx.addVar(n, check(n.value))
    types.Var("Unit")
  }

  private def check(n: nodes.Mut): Type = {
    ctx.addVar(n, check(n.value))
    types.Var("Unit")
  }

  private def check(n: nodes.Static): Type = {
    val lhs = ctx.getVar(n)
    val rhs = check(n.value)
    if (lhs != rhs)
      throw CheckerError(s"${n.pos}: Expected $lhs but found $rhs")
    types.Var("Unit")
  }

  private def check(n: nodes.Fun): Type = {
    val inner = ctx.clone()
    n.terms.foreach(collect(_, inner))
    n.body.foreach(collect(_, inner))
    val block = new Checker(inner)
    val bodyTypes = n.body.map(block.check)
    val lhs = inner.mkType(n.terms.last)
    val rhs = bodyTypes.last
    if (lhs != rhs)
      throw CheckerError(s"${n.body.last.pos}: Expected $lhs but found $rhs")
    types.Var("Unit")
  }

  private def check(n: nodes.Top): Type = {
    collect(n, ctx)
    assert(n.nodes.map(check).forall(_ == types.Var("Unit")),
      "ERROR: ALL TOP LEVEL NODES MUST BE OF TYPE UNIT")
    types.Var("Unit")
  }

  private def check(n: nodes.App): Type = {
    ctx.getApp(n, n.args map check)
  }

  private def collect(n: Node, ctx: Context): Unit = n match {
    case top@nodes.Top(ns) =>
      ns.foreach(collect(_, ctx))
    case fun@nodes.Fun(nm, ts, _) =>
      val ps = filterParam(ts).toSeq
      val pTypes = ps.map(p => ctx.mkType(p.typename))
      val rtType = ctx.mkType(ts.last)
      ctx.addAbs(fun, types.Abs(pTypes :+ rtType))
    case sta@nodes.Static(nm, typename, _) =>
      ctx.addVar(sta, ctx.mkType(typename))
    case prm@nodes.Param(nm, tn) =>
      val tp = ctx.mkType(tn)
      ctx.addVar(prm, tp)
    case other =>
      // Nothing to collect...
  }
}

object Checker {
  def check(root: Node): Type = {
    val found = new Checker().check(root)
    assert(found == types.Var("Unit"), "ERROR: TOP TYPE SHOULD BE UNIT")
    return found
  }
}
