package atlas

import atlas.ast.Node

object PartialEvaluator {
  def partEval(n: Node): Node = n match {
    case n: ast.Integer => n
    case n: ast.Boolean => n
    case n: ast.Identifier => n
    case n: ast.Assign  => n.copy(value = partEval(n.value))(n.pos)
    case n: ast.Let     => n.copy(value = partEval(n.value))(n.pos)
    case n: ast.Mut     => n.copy(value = partEval(n.value))(n.pos)
    case n: ast.Fun     => n.copy(body = n.body.map(partEval))(n.pos)
    case n: ast.Top     => n.copy(n.nodes.map(partEval))(n.pos)
    case n: ast.App     => n.copy(args = n.args.map(partEval))(n.pos)
    case n: ast.Static  => n.copy(value = partEval(n.value))(n.pos)
    case n: ast.Cons    => n.copy(args = n.args.map(partEval))(n.pos)
    case n: ast.Subscript => n.copy(arg = partEval(n.arg))(n.pos)
    case n: ast.AssignSub => ast.AssignSub(n.name,
        partEval(n.index),
        n.op,
        partEval(n.value)
      )(n.pos)
    case n: ast.For     => ast.For(n.name,
        partEval(n.from),
        partEval(n.to),
        n.body.map(partEval)
      )(n.pos)
    case n: ast.While   => ast.While(
        partEval(n.cond),
        n.body.map(partEval)
      )(n.pos)
    case n: ast.Cond    => ast.Cond(
        partEval(n.cond),
        n.body.map(partEval),
        n.others.map(partEval)
      )(n.pos)
    case n: ast.Elif    => ast.Elif(
        partEval(n.cond),
        n.body.map(partEval)
      )(n.pos)
    case n: ast.Else    => ast.Else(
        n.body.map(partEval)
      )(n.pos)
    case n: ast.BinOp   =>
      val lhs = partEval(n.lhs)
      val rhs = partEval(n.rhs)

      (lhs, rhs) match {
        case (ast.Integer(l), ast.Integer(r)) =>
          n.op match {
            case "+"  => ast.Integer(l + r)(n.pos)
            case "-"  => ast.Integer(l - r)(n.pos)
            case "/"  => ast.Integer(l / r)(n.pos)
            case "*"  => ast.Integer(l * r)(n.pos)
            case "%"  => ast.Integer(l % r)(n.pos)
            case "==" => ast.Boolean(l == r)(n.pos)
            case "!=" => ast.Boolean(l != r)(n.pos)
            case ">"  => ast.Boolean(l > r)(n.pos)
            case "<"  => ast.Boolean(l < r)(n.pos)
            case "<=" => ast.Boolean(l <= r)(n.pos)
            case ">=" => ast.Boolean(l >= r)(n.pos)
            case _ => ???
          }
        case (ast.Boolean(l), ast.Boolean(r)) =>
          n.op match {
            case "==" => ast.Boolean(l == r)(n.pos)
            case "!=" => ast.Boolean(l != r)(n.pos)
            case "or" => ast.Boolean(l || r)(n.pos)
            case "and" => ast.Boolean(l && r)(n.pos)
            case _ => ???
          }
        case _ => n.copy(lhs, n.op, rhs)(n.pos)
      }
    case n: ast.UnaOp   =>
      val rhs = partEval(n.rhs)

      rhs match {
        case ast.Integer(r) =>
          n.op match {
            case "-"  => ast.Integer(-r)(n.pos)
            case _ => ???
          }
        case ast.Boolean(r) =>
          n.op match {
            case "!" => ast.Boolean(!r)(n.pos)
            case _ => ???
          }
        case _ => n.copy(n.op, rhs)(n.pos)
      }

    case n: ast.Nop     => n
    case others         => ???
  }
}