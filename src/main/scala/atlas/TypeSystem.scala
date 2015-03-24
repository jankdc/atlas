package atlas

import atlas.ast.Node
import atlas.types.Type

object TypeSystem {
  case class Env(archive: NodeMap, context: Context)

  def collectTypes(c: Context, n: Node): NodeMap =
    check(Env(NodeMap(Map()), c), "", n) match { case (e, _) => e.archive }

  private def check(e: Env, s: String, n: Node): (Env, Type) = n match {
    case n: ast.Integer => check(e, s, n)
    case n: ast.Boolean => check(e, s, n)
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
    val v = NodeMeta(t, None)
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: String, n: ast.Boolean): (Env, Type) = {
    val t = types.Var("Boolean")
    val v = NodeMeta(t, None)
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: String, n: ast.NamedId): (Env, Type) = {
    val (sym, t) = e.context.getDef(n.name, n.pos)
    val v = NodeMeta(t, Some(sym))
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: String, n: ast.Let): (Env, Type) = {
    val sn = s + "::" + n.name
    val sm = Symbol(s, n.name)(n.pos, false, true)
    val (e1, t) = check(e, sn, n.value)
    val c = e1.context.addDef(sm, t)
    val v = NodeMeta(t, Some(sm))

    ((Env(e1.archive.add(n, v), c)), types.Var("Unit"))
  }

  private def check(e: Env, s: String, n: ast.Mut): (Env, Type) = {
    val sn = s + "::" + n.name
    val sm = Symbol(s, n.name)(n.pos, false, false)
    val (e1, t) = check(e, sn, n.value)
    val c = e1.context.addDef(sm, t)
    val v = NodeMeta(t, Some(sm))

    ((Env(e1.archive.add(n, v), c)), types.Var("Unit"))
  }

  private def check(e: Env, s: String, n: ast.Fun): (Env, Type) = {
    val s1 = s + "::" + n.name
    val e1 = n.params.foldLeft(e) { case (e, n) => collect(e, s1, n) }
    val e2 = n.body.foldLeft(e1)  { case (e, n) => collect(e, s1, n) }

    val (e3, ts) = n.body.foldLeft(e2, Seq[Type]()) {
      case ((e, ts), n) =>
        check(e, s1, n) match { case (e4, t) => (e4, ts :+ t) }
    }

    val bt = ts.last
    val rt = toType(n.ret)

    if (bt != rt) {
      println(ts)
      throw TypeError(s"${n.body.last.pos}: Expected $rt but found $bt")
    }

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

    val v = NodeMeta(t1, None)

    (e2.copy(archive = e2.archive.add(n, v)), t1)
  }

  private def check(e: Env, s: String, n: ast.Nop): (Env, Type) = {
    val t = types.Var("Unit")
    val v = NodeMeta(t, None)
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: String, n: ast.App): (Env, Type) = {
    val (e3, t1) = n.args.foldLeft(e, Seq[Type]()) {
      case ((e1, ts), n) =>
        check(e1, s, n) match { case (e2, t) => (e2, ts :+ t) }
    }
    val args = "(" + t1.mkString(", ") + ")"
    val (sm, t2) = e3.context.getDef(n.name + args, n.pos)
    val v = NodeMeta(t2, Some(sm))
    (e3.copy(archive = e3.archive.add(n, v)), t2)
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
        val exp = types.Var("Boolean")
        if (lhs != rhs)
          throw TypeError(s"${n.lhs.pos}: Expected $lhs but found $rhs")
        exp
    }

    val p = ((n, n.pos) -> NodeMeta(t, None))
    val v = NodeMeta(t, None)

    ((e2.copy(archive = e2.archive.add(n, v))), t)
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
    val p = ((n, n.pos) -> NodeMeta(t, None))
    val v = NodeMeta(t, None)
    ((e1.copy(archive = e1.archive.add(n, v))), t)
  }

  private def check(e: Env, s: String, n: ast.Static): (Env, Type) = {
    val t1 = types.Var("Unit")
    val s1 = s + "::" + n.name
    val (sm, t2) = e.context.getDef(n.name, n.pos)
    val (e1, t3) = check(e, s1, n.value)

    if (t2 != t3)
      throw TypeError(s"${n.pos}: Expected $t2 but found $t3")

    val v = NodeMeta(t1, Some(sm))
    ((e1.copy(archive = e1.archive.add(n, v))), t1)
  }

  private def collect(e: Env, s: String, n: Node): Env = n match {
    case ast.Top(ns) =>
      ns.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Fun(nm, ps, rt, _) =>
      val p = ps.map(p => toType(p.typename))
      val r = toType(rt)
      val a = "(" + p.mkString(", ") + ")"
      val b = Symbol(s, nm, a)(n.pos, true, true)
      val c = e.context.addDef(b, r)
      e.copy(context = c)
    case ast.Param(nm, tn) =>
      val t = toType(tn)
      val b = Symbol(s, nm)(n.pos, false, true)
      val c = e.context.addDef(b, t)
      e.copy(context = c)
    case ast.Static(nm, tn, _) =>
      val t = toType(tn)
      val b = Symbol(s, nm)(n.pos, true, true)
      val c = e.context.addDef(b, t)
      e.copy(context = c)
    case other =>
      e
  }

  private def toType(n: ast.Node): Type = n match {
    case ast.Type(Seq(n)) => toType(n)
    case ast.Type(many) => types.Fun(many.map(toType))
    case ast.NamedId(n) => types.Var(n)
    case other => ???
  }
}
