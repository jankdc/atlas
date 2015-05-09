package atlas

import atlas.ast.Node
import atlas.types.Type

object TypeSystem {
  case class Env(archive: NodeMap, context: Context)
  case class Scope(name: String = "", level: Int = 0)

  def collectTypes(c: Context, n: Node): NodeMap = {
    val env = Env(NodeMap(Map()), c)
    check(env, Scope(), n) match { case (e, _) => e.archive }
  }

  private def check(e: Env, s: Scope, n: Node): (Env, Type) = n match {
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
    case n: ast.Cond    => check(e, s, n)
    case n: ast.Elif    => check(e, s, n)
    case n: ast.Else    => check(e, s, n)
    case n: ast.While   => check(e, s, n)
    case n: ast.Cons    => check(e, s, n)
    case n: ast.For     => check(e, s, n)
    case n: ast.Assign  => check(e, s, n)
    case n: ast.Subscript => check(e, s, n)
    case others         => ???
  }

  private def check(e: Env, s: Scope, n: ast.Integer): (Env, Type) = {
    val t = types.Var("Int")
    val v = NodeMeta(t, None)
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: Scope, n: ast.Boolean): (Env, Type) = {
    val t = types.Var("Boolean")
    val v = NodeMeta(t, None)
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: Scope, n: ast.NamedId): (Env, Type) = {
    val (sym, t) = e.context.getDef(n.name, n.pos)
    val v = NodeMeta(t, Some(sym))
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: Scope, n: ast.Let): (Env, Type) = {
    val sn = s.name + n.name + "_"
    val sm = Symbol(s.name, n.name)(n.pos, false, true, s.level)
    val (e1, t) = check(e, s.copy(name = sn), n.value)
    val c = e1.context.addDef(sm, t)
    val v = NodeMeta(types.Var("Unit"), Some(sm))

    ((Env(e1.archive.add(n, v), c)), types.Var("Unit"))
  }

  private def check(e: Env, s: Scope, n: ast.Mut): (Env, Type) = {
    val sn = s.name + n.name + "_"
    val sm = Symbol(s.name, n.name)(n.pos, false, false, s.level)
    val (e1, t) = check(e, s.copy(name = sn), n.value)
    val c = e1.context.addDef(sm, t)
    val v = NodeMeta(types.Var("Unit"), Some(sm))

    ((Env(e1.archive.add(n, v), c)), types.Var("Unit"))
  }

  private def check(e: Env, s: Scope, n: ast.For): (Env, Type) = {
    val sn = s.name + n.name + "_"
    val newScope = s.copy(name = sn, level = s.level + 1)
    val sm = Symbol(s.name, n.name)(n.pos, false, true, s.level)
    val (e1, ft) = check(e, s.copy(name = sn),  n.from)
    val (e2, tt) = check(e1, s.copy(name = sn), n.to)
    val intTp = types.Var("Int")

    if (ft != intTp)
      throw TypeError(s"${n.from.pos}: Expected $intTp but found $ft")

    if (tt != intTp)
      throw TypeError(s"${n.to.pos}: Expected $intTp but found $tt")

    val c = e1.context.addDef(sm, intTp)
    val v = NodeMeta(types.Var("Unit"), Some(sm))

    val (e3, bodyTypes) = n.body.foldLeft(e2.copy(context=c), Seq[Type]()) {
      case ((e, ts), n) =>
        val (newEnv, t) = check(e, newScope, n)
        (newEnv, ts :+ t)
    }

    (Env(e3.archive.add(n, v), e.context), types.Var("Unit"))
  }

  private def check(e: Env, s: Scope, n: ast.Assign): (Env, Type) = {
    val (sym, tn) = e.context.getDef(n.name, n.pos)

    if (sym.isConstant)
      throw TypeError(s"${n.pos}: ${n.name} is immutable and cannot be assigned.")

    val sn = s.name + n.name + "_"
    val (e1, tv) = check(e, s.copy(name = sn), n.value)

    (tn, n.op, tv) match {
      case (types.List(item), "+=", tv) =>
        if (item != tv)
          throw TypeError(s"${n.value.pos}: Expected $item but found $tv")
      case _ =>
        if (tn != tv)
          throw TypeError(s"${n.value.pos}: Expected $tn but found $tv")
    }

    val t = types.Var("Unit")
    val v = NodeMeta(t, Some(sym))

    (e1.copy(archive = e1.archive.add(n, v)), t)
  }

  private def check(e: Env, s: Scope, n: ast.Fun): (Env, Type) = {
    val s1 = s.name + n.name + "_"
    val b1 = e.context.bindings.filterNot {
      case (sym, types.Var(_)) => ! sym.isStatic
      case _ => false
    }

    val e1 = n.params.foldLeft(
      e.copy(context = e.context.copy(bindings = b1))) {
      case (e, n) => collect(e, Scope(s1, s.level + 1), n)
    }

    val e2 = n.body.foldLeft(e1)  {
      case (e, n) => collect(e, Scope(s1, s.level + 1), n)
    }
    val (e3, ts) = n.body.foldLeft(e2, Seq[Type]()) {
      case ((e, ts), n) =>
        check(e, Scope(s1, s.level + 1), n) match {
          case (e4, t) => (e4, ts :+ t)
        }
    }

    val paramNames = n.params.map(p => p.name)
    val returnParm = returnsParam(n.body.last, paramNames)

    println(n.name, returnParm, n.body.last)

    val bt = (n.body, ts).zipped.toList
     .filterNot { case (p, _) => p.isInstanceOf[ast.Fun] }
     .filterNot { case (p, _) => p.isInstanceOf[ast.Static] }
     .map(_._2).last
    val rt = toType(n.ret)
    val t1 = types.Var("Unit")
    val sm = Symbol(s.name, n.name)(n.pos, true, true, s.level, returnParm)
    val e4 = e3.copy(archive = e3.archive.add(n.ret, NodeMeta(rt, Some(sm))))

    if (bt != rt) {
      throw TypeError(s"${n.body.last.pos}: Expected $rt but found $bt")
    }

    (e4.copy(context = e.context), types.Var("Unit"))
  }

  private def returnsParam(n: Node, paramNames: Seq[String]): Boolean = n match {
    case ast.NamedId(nm) => paramNames contains nm
    case ast.Cond(_, body, others) =>
      returnsParam(body.last, paramNames) && others.exists(returnsParam(_, paramNames))
    case ast.Elif(_, body) =>
      returnsParam(body.last, paramNames)
    case ast.Else(body) =>
      returnsParam(body.last, paramNames)
    case _ => false
  }

  private def check(e: Env, s: Scope, n: ast.Top): (Env, Type) = {
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

  private def check(e: Env, s: Scope, n: ast.Nop): (Env, Type) = {
    val t = types.Var("Unit")
    val v = NodeMeta(t, None)
    (e.copy(archive = e.archive.add(n, v)), t)
  }

  private def check(e: Env, s: Scope, n: ast.App): (Env, Type) = {
    val (e3, t1) = n.args.foldLeft(e, Seq[Type]()) {
      case ((e1, ts), n) =>
        check(e1, s, n) match { case (e2, t) => (e2, ts :+ t) }
    }
    val args = "(" + t1.mkString(", ") + ")"
    val (sm, t2) = e3.context.getDef(n.name + args, n.pos)
    val v = NodeMeta(t2, Some(sm))
    (e3.copy(archive = e3.archive.add(n, v)), t2)
  }

  private def check(e: Env, s: Scope, n: ast.BinOp): (Env, Type) = {
    val (e1, lhs) = check(e , s, n.lhs)
    val (e2, rhs) = check(e1, s, n.rhs)

    val t = n.op match {
      case "+" | "-" | "/" | "*" | "%" =>
        val exp = types.Var("Int")
        if (lhs != exp)
          throw TypeError(s"${n.lhs.pos}: Expected $exp but found $lhs")
        if (rhs != exp)
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case "==" | "!=" =>
        val exp = types.Var("Boolean")
        if (lhs != rhs)
          throw TypeError(s"${n.lhs.pos}: Expected $lhs but found $rhs")
        exp
      case "<" | ">" | "<=" | ">=" =>
        val exp = types.Var("Boolean")
        if (lhs != types.Var("Int"))
          throw TypeError(s"${n.lhs.pos}: Expected $exp but found $lhs")
        if (rhs != types.Var("Int"))
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case "or" | "and" =>
        val exp = types.Var("Boolean")
        if (lhs != exp)
          throw TypeError(s"${n.lhs.pos}: Expected $exp but found $lhs")
        if (rhs != exp)
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
    }

    val p = ((n, n.pos) -> NodeMeta(t, None))
    val v = NodeMeta(t, None)

    ((e2.copy(archive = e2.archive.add(n, v))), t)
  }

  private def check(e: Env, s: Scope, n: ast.UnaOp): (Env, Type) = {
    val (e1, rhs) = check(e, s, n.rhs)
    val t = n.op match {
      case "-" =>
        val exp = types.Var("Int")
        if (rhs != exp)
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case "!" =>
        val exp = types.Var("Boolean")
        if (rhs != exp)
          throw TypeError(s"${n.rhs.pos}: Expected $exp but found $rhs")
        exp
      case _ => ???
    }
    val p = ((n, n.pos) -> NodeMeta(t, None))
    val v = NodeMeta(t, None)
    ((e1.copy(archive = e1.archive.add(n, v))), t)
  }

  private def check(e: Env, s: Scope, n: ast.Static): (Env, Type) = {
    val t1 = types.Var("Unit")
    val s1 = s.name + n.name + "_"
    val (sm, t2) = e.context.getDef(n.name, n.pos)
    val (e1, t3) = check(e, s.copy(name = s1), n.value)

    if (t2 != t3)
      throw TypeError(s"${n.pos}: Expected $t2 but found $t3")

    val v = NodeMeta(t1, Some(sm))
    ((e1.copy(archive = e1.archive.add(n, v))), t1)
  }

  private def check(e: Env, s: Scope, n: ast.Cond): (Env, Type) = {
    val s1 = s.name + "_"
    val newScope = s.copy(name = s1, level = s.level + 1)
    val cetype = types.Var("Boolean")
    val (e1, catype) = check(e, s, n.cond)

    if (catype != cetype)
      throw TypeError(s"${n.cond.pos}: Expected $cetype but found $catype")

    val e2 = n.body.foldLeft(e1) { case (e, n) => collect(e, newScope, n) }

    val (e3, bodyTypes) = n.body.foldLeft(e2, Seq[Type]()) {
      case ((e, ts), n) =>
        val (newEnv, t) = check(e, newScope, n)
        (newEnv, ts :+ t)
    }

    val t = bodyTypes.last

    val (e4, otherTypes) = n.others.foldLeft(
      e3.copy(context = e.context), Seq[Type]()) {
      case ((e, ts), n) =>
        val (newEnv, t) = check(e, s, n)
        (newEnv, ts :+ t)
    }

    val isSameTypes = otherTypes.forall(_ == t)
    val hasElseStmt = n.others.exists(_.isInstanceOf[ast.Else])

    if (isSameTypes && otherTypes.nonEmpty && hasElseStmt) {
      val v = NodeMeta(t, None)
      (Env(e4.archive.add(n, v), e.context), t)
    }
    else {
      val v = NodeMeta(types.Var("Unit"), None)
      (Env(e4.archive.add(n, v), e.context), types.Var("Unit"))
    }
  }

  private def check(e: Env, s: Scope, n: ast.Else): (Env, Type) = {
    val s1 = s.name + "_"
    val newScope = s.copy(name = s1, level = s.level + 1)
    val e1 = n.body.foldLeft(e) { case (e, n) => collect(e, newScope, n) }
    val (e2, bodyTypes) = n.body.foldLeft(e1, Seq[Type]()) {
      case ((e, ts), n) =>
        val (newEnv, t) = check(e, newScope, n)
        (newEnv, ts :+ t)
    }

    val t = bodyTypes.last
    val v = NodeMeta(t, None)

    (Env(e2.archive.add(n, v), e.context), t)
  }

  private def check(e: Env, s: Scope, n: ast.Elif): (Env, Type) = {
    val s1 = s.name + "_"
    val newScope = s.copy(name = s1, level = s.level + 1)
    val cetype = types.Var("Boolean")
    val (e1, catype) = check(e, s, n.cond)

    if (catype != cetype)
      throw TypeError(s"${n.cond.pos}: Expected $cetype but found $catype")

    val e2 = n.body.foldLeft(e1) { case (e, n) => collect(e, newScope, n) }

    val (e3, bodyTypes) = n.body.foldLeft(e2, Seq[Type]()) {
      case ((e, ts), n) =>
        val (newEnv, t) = check(e, newScope, n)
        (newEnv, ts :+ t)
    }

    val t = bodyTypes.last
    val v = NodeMeta(t, None)

    (Env(e3.archive.add(n, v), e.context), t)
  }

  private def check(e: Env, s: Scope, n: ast.While): (Env, Type) = {
    val s1 = s.name + "_"
    val newScope = s.copy(name = s1, level = s.level + 1)
    val cetype = types.Var("Boolean")
    val (e1, catype) = check(e, s, n.cond)

    if (catype != cetype)
      throw TypeError(s"${n.cond.pos}: Expected $cetype but found $catype")

    val e2 = n.body.foldLeft(e1) { case (e, n) => collect(e, newScope, n) }

    val (e3, bodyTypes) = n.body.foldLeft(e2, Seq[Type]()) {
      case ((e, ts), n) =>
        val (newEnv, t) = check(e, newScope, n)
        (newEnv, ts :+ t)
    }

    val t = types.Var("Unit")
    val v = NodeMeta(t, None)

    (Env(e3.archive.add(n, v), e.context), t)
  }

  private def check(e: Env, s: Scope, n: ast.Cons): (Env, Type) = {
    val tp = toType(n.typeid)
    val e1 = n.args.foldLeft(e) { case (e, arg) =>
      val (newEnv, argTp) = check(e, s, arg)
      if (tp != argTp)
        throw TypeError(s"${arg.pos}: Expected $tp but found $argTp")

      newEnv
    }

    val t = types.List(tp)
    val v = NodeMeta(t, None)
    (e1.copy(archive = e1.archive.add(n, v)), t)
  }

  private def check(e: Env, s: Scope, n: ast.Subscript): (Env, Type) = {
    val (sm, tp@types.List(itemTp)) = e.context.getDef(n.name, n.pos)

    val e1 = tp match {
      case types.List(_) =>
        val (newEnv, acc) = check(e, s, n.arg)
        if (acc != types.Var("Int"))
          throw TypeError(s"${n.pos}: Expected Int but found $acc")
        newEnv
      case _ =>
        ???
    }

    val t = itemTp
    val v = NodeMeta(t, Some(sm))
    (e1.copy(archive = e1.archive.add(n, v)), t)
  }

  private def collect(e: Env, s: Scope, n: Node): Env = n match {
    case ast.Top(ns) =>
      ns.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Fun(nm, ps, rt, bd) =>
      val p = ps.map(p => toType(p.typename))
      val r = toType(rt)
      val paramNames = ps.map(p => p.name)
      val returnParm = returnsParam(bd.last, paramNames)
      val a = "(" + p.mkString(", ") + ")"
      val b = Symbol(s.name, nm, a)(n.pos, true, true, s.level, returnParm)
      val c = e.context.addDef(b, r)
      e.copy(e.archive.add(n, NodeMeta(types.Var("Unit"), Some(b))), c)
    case ast.Cond(_, bd, _) =>
      bd.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Elif(_, bd) =>
      bd.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Else(bd) =>
      bd.foldLeft(e) { case (e, n) => collect(e, s, n) }
    case ast.Param(nm, tn) =>
      val t = toType(tn)
      val b = Symbol(s.name, nm)(n.pos, false, true, s.level)
      val c = e.context.addDef(b, t)
      e.copy(e.archive.add(n, NodeMeta(t, Some(b))), c)
    case ast.Static(nm, tn, _) =>
      val t = toType(tn)
      val b = Symbol(s.name, nm)(n.pos, true, true, s.level)
      val c = e.context.addDef(b, t)
      e.copy(context = c)
    case other =>
      e
  }

  private def toType(n: ast.Node): Type = n match {
    case ast.Type(Seq(n)) => toType(n)
    case ast.Type(many)   => types.Fun(many.map(toType))
    case ast.NamedId(n)   => types.Var(n)
    case ast.ListType(tp) => types.List(toType(tp))
    case other => ???
  }
}
