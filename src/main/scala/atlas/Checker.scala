package atlas

import atlas.ast.Node
import atlas.types.Type

object findType {
  def apply(e: Env, n: Node): (Env, Type) = check(e, "", n)

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
    (e.copy(archive = e.archive + ((n, n.pos) -> t)), t)
  }

  private def check(e: Env, s: String, n: ast.NamedId): (Env, Type) = {
    val t = e.context.getVar(Sym(s, n.name)(n.pos))
    (e.copy(archive = e.archive + ((n, n.pos) -> t)), t)
  }

  private def check(e: Env, s: String, n: ast.Let): (Env, Type) = {
    val s1 = s + "::" + n.name
    val (e1, t) = check(e, s1, n.value)
    val c1 = e.context.addVar(Sym(s, n.name)(n.pos), t)
    val t1 = types.Var("Unit")

    ((Env(e.archive + ((n, n.pos) -> t1), c1)), t1)
  }

  private def check(e: Env, s: String, n: ast.Mut): (Env, Type) = {
    val s1 = s + "::" + n.name
    val (e1, t) = check(e, s1, n.value)
    val c1 = e.context.addVar(Sym(s, n.name)(n.pos), t)
    val t1 = types.Var("Unit")

    ((Env(e.archive + ((n, n.pos) -> t1), c1)), t1)
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
      throw CheckError(s"${n.body.last.pos}: Expected $rt but found $bt")

    (e3.copy(context = e.context), types.Var("Unit"))
  }

  private def check(e: Env, s: String, n: ast.Top): (Env, Type) = {
    val t1 = types.Var("Unit")
    val e1 = collect(e, s, n)
    val e2 = n.nodes.foldLeft(e1) { case (e, n) =>
      val (e1, t2) = check(e, s, n)
      if (t1 != t2)
        throw CheckError(s"${n.pos}: Expected $t1 but found $t2")
      e1
    }
    (e2, t1)
  }

  private def check(e: Env, s: String, n: ast.Static): (Env, Type) = {
    val t1 = types.Var("Unit")
    val s1 = s + "::" + n.name
    val t2 = e.context.getVar(Sym(s, n.name)(n.pos))
    val (e1, t3) = check(e, s1, n.value)

    if (t2 != t3)
      throw CheckError(s"${n.pos}: Expected $t2 but found $t3")

    ((e1.copy(archive = e.archive + ((n, n.pos) -> t1))), t1)
  }

  private def check(e: Env, s: String, n: ast.BinOp): (Env, Type) = {
    val (e1, lhs) = check(e , s, n.lhs)
    val (e2, rhs) = check(e1, s, n.rhs)
    val t = n.op match {
      case "+" | "-" | "/" | "*" =>
        val exp = types.Var("Int")
        if (lhs != exp)
          throw CheckError(s"${n.lhs.pos}: Expected $exp but found $lhs")
        if (rhs != exp)
          throw CheckError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
        val exp = types.Var("Bool")
        if (lhs != rhs)
          throw CheckError(s"${n.lhs.pos}: Expected $lhs but found $rhs")
        exp
    }

    ((e2.copy(archive = e2.archive + ((n, n.pos) -> t))), t)
  }

  private def check(e: Env, s: String, n: ast.UnaOp): (Env, Type) = {
    val (e1, rhs) = check(e, s, n.rhs)
    val t = n.op match {
      case "-" =>
        val exp = types.Var("Int")
        if (rhs != exp)
          throw CheckError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case not => ???
    }
    ((e1.copy(archive = e1.archive + ((n, n.pos) -> t))), t)
  }

  private def check(e: Env, s: String, n: ast.Nop): (Env, Type) = {
    val t = types.Var("Unit")
    (e.copy(archive = e.archive + ((n, n.pos) -> t)), t)
  }

  private def check(e: Env, s: String, n: ast.App): (Env, Type) = {
    val (e2, t1) = n.args.foldLeft(e, Seq[Type]()) { case ((env, ts), n) =>
      val (e2, t) = check(env, s, n)
      (e2, ts :+ t)
    }
    val (s1, t2) = e2.context.getFun(Sym(s, n.name)(n.pos), t1)
    (e.copy(archive = e.archive + ((n, n.pos) -> types.App(s1, t2))), t2)
  }

  private def collect(e: Env, s: String, n: Node): Env = n match {
    case ast.Top(ns) =>
      ns.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Fun(nm, ps, rt, _) =>
      val p = ps.map(p => toType(p.typename))
      val r = toType(rt)
      val c = e.context.addFun(Sym(s, nm)(n.pos), types.Fun(p :+ r))
      e.copy(context = c)
    case ast.Param(nm, tn) =>
      val t = toType(tn)
      val c = e.context.addVar(Sym(s, nm)(n.pos), t)
      e.copy(context = c)
    case ast.Static(nm, tpn, _) =>
      val t = toType(tpn)
      val c = e.context.addVar(Sym(s, nm)(n.pos), t)
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

// class Checker(outer: Context = new Context()) {
//   val ctx = outer.clone()

//   def check(n: Node): Type = n match {
//     case n: ast.Integer => types.Var("Int")
//     case n: ast.NamedId  => ctx.getVar(n)
//     case n: ast.Let     => check(n)
//     case n: ast.Mut     => check(n)
//     case n: ast.Fun     => check(n)
//     case n: ast.Top     => check(n)
//     case n: ast.App     => check(n)
//     case n: ast.Static  => check(n)
//     case n: ast.BinOp   => check(n)
//     case n: ast.UnaOp   => check(n)
//     case n: ast.Nop     => types.Var("Unit")
//     case others           => ???
//   }

//   private def check(n: ast.UnaOp): Type =
//     check(ast.BinOp(ast.Integer(1)(n.pos), n.op, n.rhs)(n.pos))

//   private def check(n: ast.BinOp): Type = {
//     n.op match {
//       case "+" | "-" | "/" | "*" =>
//         val exp = types.Var("Int")
//         (check(n.lhs), check(n.rhs)) match {
//           case (lhs, rhs) if lhs == exp && rhs == exp =>
//             exp
//           case (lhs, rhs) if lhs == exp =>
//             throw CheckError(s"${n.rhs.pos}: Expected $exp but found $rhs")
//           case (lhs, rhs) if rhs == exp =>
//             throw CheckError(s"${n.lhs.pos}: Expected $exp but found $lhs")
//         }
//       case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
//         val exp = types.Var("Bool")
//         (check(n.lhs), check(n.rhs)) match {
//           case (lhs, rhs) if lhs == rhs =>
//             exp
//           case (lhs, rhs) =>
//             throw CheckError(s"${n.lhs.pos}: Expected $lhs but found $rhs")
//         }
//     }
//   }

//   private def check(n: ast.Let): Type = {
//     ctx.addVar(n, check(n.value))
//     types.Var("Unit")
//   }

//   private def check(n: ast.Mut): Type = {
//     ctx.addVar(n, check(n.value))
//     types.Var("Unit")
//   }

//   private def check(n: ast.Static): Type = {
//     val lhs = ctx.getVar(n)
//     val rhs = check(n.value)
//     if (lhs != rhs)
//       throw CheckError(s"${n.pos}: Expected $lhs but found $rhs")
//     types.Var("Unit")
//   }

//   private def check(n: ast.Fun): Type = {
//     val inner = ctx.clone()
//     n.params.foreach(collect(_, inner))
//     n.body.foreach(collect(_, inner))
//     val block = new Checker(inner)
//     val bodyTypes = n.body.map(block.check)
//     val lhs = inner.mkType(n.ret)
//     val rhs = bodyTypes.last
//     if (lhs != rhs)
//       throw CheckError(s"${n.body.last.pos}: Expected $lhs but found $rhs")
//     types.Var("Unit")
//   }

//   private def check(n: ast.Top): Type = {
//     collect(n, ctx)
//     assert(n.nodes.map(check).forall(_ == types.Var("Unit")),
//       "ERROR: ALL TOP LEVEL NODES MUST BE OF TYPE UNIT")
//     types.Var("Unit")
//   }

//   private def check(n: ast.App): Type = {
//     ctx.getApp(n, n.args map check)
//   }

//   private def collect(n: Node, ctx: Context): Unit = n match {
//     case n@ast.Top(ns) =>
//       ns.foreach(collect(_, ctx))
//     case n@ast.Fun(nm, ps, rt, _) =>
//       val pTypes = ps.map(p => ctx.mkType(p.typename))
//       val rtType = ctx.mkType(rt)
//       ctx.addAbs(n, types.Abs(pTypes :+ rtType))
//     case n@ast.Static(nm, typename, _) =>
//       ctx.addVar(n, ctx.mkType(typename))
//     case n@ast.Param(nm, tn) =>
//       val tp = ctx.mkType(tn)
//       ctx.addVar(n, tp)
//     case other =>
//       // Nothing to collect...
//   }
// }

// object Checker {
//   def check(root: Node): Type = {
//     val found = new Checker().check(root)
//     assert(found == types.Var("Unit"), "ERROR: TOP TYPE SHOULD BE UNIT")
//     return found
//   }
// }
