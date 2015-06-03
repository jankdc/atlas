package atlas

import atlas.ast.Node

object partEval {
  def apply(n: Node): Node = n match {
    case n: ast.Integer =>
      n
    case n: ast.Boolean =>
      n
    case n: ast.Identifier =>
      n
    case n: ast.Assign =>
      n.copy(value = apply(n.value))(n.pos)
    case n: ast.Let =>
      n.copy(value = apply(n.value))(n.pos)
    case n: ast.Mut =>
      n.copy(value = apply(n.value))(n.pos)
    case n: ast.Fun =>
      n.copy(body = n.body.map(apply))(n.pos)
    case n: ast.Top =>
      n.copy(n.nodes.map(apply))(n.pos)
    case n: ast.App =>
      n.copy(args = n.args.map(apply))(n.pos)
    case n: ast.Static =>
      n.copy(value = apply(n.value))(n.pos)
    case n: ast.Cons =>
      n.copy(args = n.args.map(apply))(n.pos)
    case n: ast.Subscript =>
      n.copy(arg = apply(n.arg))(n.pos)
    case n: ast.AssignSub =>
      ast.AssignSub(n.name, apply(n.index), n.op, apply(n.value))(n.pos)
    case n: ast.For =>
      ast.For(n.name, apply(n.from), apply(n.to), n.body.map(apply))(n.pos)
    case n: ast.While =>
      ast.While(apply(n.cond), n.body.map(apply))(n.pos)
    case n: ast.Cond =>
      ast.Cond(apply(n.cond), n.body.map(apply), n.others.map(apply))(n.pos)
    case n: ast.Elif =>
      ast.Elif(apply(n.cond), n.body.map(apply))(n.pos)
    case n: ast.Else =>
      ast.Else(n.body.map(apply))(n.pos)
    case n: ast.BinOp =>
      (apply(n.lhs), apply(n.rhs)) match {
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
        case (lhs, rhs) => n.copy(lhs, n.op, rhs)(n.pos)
      }
    case n: ast.UnaOp   =>
      apply(n.rhs) match {
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
        case rhs => n.copy(n.op, rhs)(n.pos)
      }
    case n: ast.Nop =>
      n
    case _ =>
      ???
  }
}
