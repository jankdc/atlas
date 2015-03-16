package atlas

import atlas.ast.Node
import atlas.types.Type

class Checker(outer: Context = new Context()) {
  val ctx = outer.clone()

  def check(n: Node): Type = n match {
    case n: ast.Integer => types.Var("Int")
    case n: ast.NamedId  => ctx.getVar(n)
    case n: ast.Let     => check(n)
    case n: ast.Mut     => check(n)
    case n: ast.Fun     => check(n)
    case n: ast.Top     => check(n)
    case n: ast.App     => check(n)
    case n: ast.Static  => check(n)
    case n: ast.BinOp   => check(n)
    case n: ast.UnaOp   => check(n)
    case n: ast.Nop     => types.Var("Unit")
    case others           => ???
  }

  private def check(n: ast.UnaOp): Type =
    check(ast.BinOp(ast.Integer(1)(n.pos), n.op, n.rhs)(n.pos))

  private def check(n: ast.BinOp): Type = {
    n.op match {
      case "+" | "-" | "/" | "*" =>
        val exp = types.Var("Int")
        (check(n.lhs), check(n.rhs)) match {
          case (lhs, rhs) if lhs == exp && rhs == exp =>
            exp
          case (lhs, rhs) if lhs == exp =>
            throw CheckError(s"${n.rhs.pos}: Expected $exp but found $rhs")
          case (lhs, rhs) if rhs == exp =>
            throw CheckError(s"${n.lhs.pos}: Expected $exp but found $lhs")
        }
      case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
        val exp = types.Var("Bool")
        (check(n.lhs), check(n.rhs)) match {
          case (lhs, rhs) if lhs == rhs =>
            exp
          case (lhs, rhs) =>
            throw CheckError(s"${n.lhs.pos}: Expected $lhs but found $rhs")
        }
    }
  }

  private def check(n: ast.Let): Type = {
    ctx.addVar(n, check(n.value))
    types.Var("Unit")
  }

  private def check(n: ast.Mut): Type = {
    ctx.addVar(n, check(n.value))
    types.Var("Unit")
  }

  private def check(n: ast.Static): Type = {
    val lhs = ctx.getVar(n)
    val rhs = check(n.value)
    if (lhs != rhs)
      throw CheckError(s"${n.pos}: Expected $lhs but found $rhs")
    types.Var("Unit")
  }

  private def check(n: ast.Fun): Type = {
    val inner = ctx.clone()
    n.terms.foreach(collect(_, inner))
    n.body.foreach(collect(_, inner))
    val block = new Checker(inner)
    val bodyTypes = n.body.map(block.check)
    val lhs = inner.mkType(n.terms.last)
    val rhs = bodyTypes.last
    if (lhs != rhs)
      throw CheckError(s"${n.body.last.pos}: Expected $lhs but found $rhs")
    types.Var("Unit")
  }

  private def check(n: ast.Top): Type = {
    collect(n, ctx)
    assert(n.nodes.map(check).forall(_ == types.Var("Unit")),
      "ERROR: ALL TOP LEVEL NODES MUST BE OF TYPE UNIT")
    types.Var("Unit")
  }

  private def check(n: ast.App): Type = {
    ctx.getApp(n, n.args map check)
  }

  private def collect(n: Node, ctx: Context): Unit = n match {
    case top@ast.Top(ns) =>
      ns.foreach(collect(_, ctx))
    case fun@ast.Fun(nm, ts, _) =>
      val ps = ts.collect { case n: ast.Param => n }
      val pTypes = ps.map(p => ctx.mkType(p.typename))
      val rtType = ctx.mkType(ts.last)
      ctx.addAbs(fun, types.Abs(pTypes :+ rtType))
    case sta@ast.Static(nm, typename, _) =>
      ctx.addVar(sta, ctx.mkType(typename))
    case prm@ast.Param(nm, tn) =>
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
