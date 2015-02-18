package atlas

import types.Type
import nodes.Node

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

  private def check(n: nodes.Fun): Type = {
    val inner = ctx.clone()
    val termTypes = n.terms.map {
      case param@nodes.Param(nm, tn) =>
        inner.addVar(param, inner.mkType(param))
        inner.mkType(param)
      case retval@nodes.NameId(nm) =>
        inner.mkType(retval)
      case others =>
        assert(false, "ERROR: UNKNOWN PARAM NODE")
    }

    val block = new Checker(inner)
    val bodyTypes = n.body.map(block.check)
    val lhs = termTypes.last
    val rhs = bodyTypes.last

    if (lhs != rhs)
      throw CheckerError(s"${n.body.last.pos}: Expected $lhs but found $rhs")

    types.Var("Unit")
  }

  private def check(n: nodes.Top): Type = {
    n.nodes.foreach {
      case fun@nodes.Fun(nm, ts, _) =>
        ctx.addAbs(fun, types.Abs(ts map ctx.mkType))
      case let@nodes.Let(nm, value) =>
        ctx.addVar(let, check(value))
      case others =>
        assert(false, "ERROR: UNKNOWN TOP LEVEL NODE")
    }

    assert(n.nodes.map(check).forall(_ == types.Var("Unit")),
      "ERROR: ALL TOP LEVEL NODES MUST BE OF TYPE UNIT")

    types.Var("Unit")
  }

  private def check(n: nodes.App): Type = {
    ctx.getApp(n, n.args map check)
  }
}

object Checker {
  def check(root: Node): Type = {
    val found = new Checker().check(root)
    assert(found == types.Var("Unit"), "ERROR: TOP TYPE SHOULD BE UNIT")
    return found
  }
}
