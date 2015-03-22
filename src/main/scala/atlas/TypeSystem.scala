package atlas

import atlas.ast.Node
import atlas.types.Type
import atlas.meta.NodeMeta

object TypeSystem {
  def collectTypes(e: Env, n: Node): (Env, Type) = check(e, "", n)

  private def check(e: Env, s: String, n: Node): (Env, Type) = n match {
    case n: ast.Integer => check(e, s, n)
    case n: ast.NamedId => check(e, s, n)
    case n: ast.Let     => check(e, s, n)
    case n: ast.Mut     => check(e, s, n)
    case n: ast.Fun     => check(e, s, n)
    case n: ast.Top     => check(e, s, n)
    case n: ast.App     => check(e, s, n)
    case n: ast.Static  => check(e, s, n)
    case n: ast.BinOp   => check(e, s, n)
    case n: ast.UnaOp   => check(e, s, n)
    case n: ast.Nop     => check(e, s, n)
    case others         => ???
  }

  private def check(e: Env, s: String, n: ast.Integer): (Env, Type) = {
    val t = types.Var("Int")
    val v = ((n, n.pos) -> NodeMeta(t, ""))
    (e.copy(archive = e.archive + v), t)
  }

  private def check(e: Env, s: String, n: ast.NamedId): (Env, Type) = {
    val t = e.context.getVar(Symbol(s, n.name)(n.pos))
    val v = ((n, n.pos) -> NodeMeta(t, ""))
    (e.copy(archive = e.archive + v), t)
  }

  private def check(e: Env, s: String, n: ast.Let): (Env, Type) = {
    val s1 = s + "::" + n.name
    val (e1, t) = check(e, s1, n.value)
    val c1 = e.context.addVar(Symbol(s, n.name)(n.pos), t)
    val t1 = types.Var("Unit")
    val va = ((n, n.pos) -> NodeMeta(t1, ""))

    ((Env(e.archive + va, c1)), t1)
  }

  private def check(e: Env, s: String, n: ast.Mut): (Env, Type) = {
    val s1 = s + "::" + n.name
    val (e1, t) = check(e, s1, n.value)
    val c1 = e.context.addVar(Symbol(s, n.name)(n.pos), t)
    val t1 = types.Var("Unit")
    val va = ((n, n.pos) -> NodeMeta(t1, ""))

    ((Env(e.archive + va, c1)), t1)
  }

  private def check(e: Env, s: String, n: ast.Fun): (Env, Type) = {
    val s1 = s + "::" + n.name
    val e1 = n.params.foldLeft(e) { case (e, n) => collect(e, s1, n) }
    val e2 = n.body.foldLeft(e1)  { case (e, n) => collect(e, s1, n) }

    val (e3, ts) = n.body.foldLeft(e2, Seq[Type]()) { case ((e, ts), n) =>
      val (e4, t) = check(e, s1, n)
      (e4, ts :+ t)
    }

    val bt = ts.last
    val rt = toType(n.ret)

    if (bt != rt)
      throw TypeError(s"${n.body.last.pos}: Expected $rt but found $bt")

    (e3.copy(context = e.context), types.Var("Unit"))
  }

  private def check(e: Env, s: String, n: ast.Top): (Env, Type) = {
    val t1 = types.Var("Unit")
    val e1 = collect(e, s, n)
    val e2 = n.nodes.foldLeft(e1) { case (e, n) =>
      val (e1, t2) = check(e, s, n)
      if (t1 != t2)
        throw TypeError(s"${n.pos}: Expected $t1 but found $t2")
      e1
    }
    (e2, t1)
  }

  private def check(e: Env, s: String, n: ast.Nop): (Env, Type) = {
    val t = types.Var("Unit")
    val v = ((n, n.pos) -> NodeMeta(t, ""))
    (e.copy(archive = e.archive + v), t)
  }

  private def check(e: Env, s: String, n: ast.App): (Env, Type) = {
    val (e2, t1) = n.args.foldLeft(e, Seq[Type]()) { case ((env, ts), n) =>
      val (e2, t) = check(env, s, n)
      (e2, ts :+ t)
    }
    val (s1, t2) = e2.context.getFun(Symbol(s, n.name)(n.pos), t1)
    val v = ((n, n.pos) -> NodeMeta(t2, s1))
    (e.copy(archive = e.archive + v), t2)
  }

  private def check(e: Env, s: String, n: ast.BinOp): (Env, Type) = {
    val (e1, lhs) = check(e , s, n.lhs)
    val (e2, rhs) = check(e1, s, n.rhs)
    val t = n.op match {
      case "+" | "-" | "/" | "*" =>
        val exp = types.Var("Int")
        if (lhs != exp)
          throw TypeError(s"${n.lhs.pos}: Expected $exp but found $lhs")
        if (rhs != exp)
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
        val exp = types.Var("Bool")
        if (lhs != rhs)
          throw TypeError(s"${n.lhs.pos}: Expected $lhs but found $rhs")
        exp
    }
    val v = ((n, n.pos) -> NodeMeta(t, ""))
    ((e2.copy(archive = e2.archive + v)), t)
  }

  private def check(e: Env, s: String, n: ast.UnaOp): (Env, Type) = {
    val (e1, rhs) = check(e, s, n.rhs)
    val t = n.op match {
      case "-" =>
        val exp = types.Var("Int")
        if (rhs != exp)
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case not => ???
    }

    val v = ((n, n.pos) -> NodeMeta(t, ""))
    ((e1.copy(archive = e1.archive + v)), t)
  }

  private def check(e: Env, s: String, n: ast.Static): (Env, Type) = {
    val t1 = types.Var("Unit")
    val s1 = s + "::" + n.name
    val t2 = e.context.getVar(Symbol(s, n.name)(n.pos))
    val (e1, t3) = check(e, s1, n.value)

    if (t2 != t3)
      throw TypeError(s"${n.pos}: Expected $t2 but found $t3")

    val v = ((n, n.pos) -> NodeMeta(t1, ""))
    ((e1.copy(archive = e1.archive + v)), t1)
  }

  private def collect(e: Env, s: String, n: Node): Env = n match {
    case ast.Top(ns) =>
      ns.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Fun(nm, ps, rt, _) =>
      val p = ps.map(p => toType(p.typename))
      val r = toType(rt)
      val c = e.context.addFun(Symbol(s, nm)(n.pos), types.Fun(p :+ r))
      e.copy(context = c)
    case ast.Param(nm, tn) =>
      val t = toType(tn)
      val c = e.context.addVar(Symbol(s, nm)(n.pos), t)
      e.copy(context = c)
    case ast.Static(nm, tpn, _) =>
      val t = toType(tpn)
      val c = e.context.addVar(Symbol(s, nm)(n.pos), t)
      e.copy(context = c)
    case other => e
  }

  private def toType(n: ast.Node): Type = n match {
    case ast.Type(Seq(ast.NamedId(nm))) => types.Var(nm)
    case ast.Type(many) => types.Fun(many.map(toType))
    case ast.NamedId(n) => types.Var(n)
    case other => ???
  }
}
