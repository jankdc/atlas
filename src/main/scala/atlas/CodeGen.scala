package atlas

import atlas.ast.Node
import atlas.types.Type

object CodeGen {
  type NodeMap = Map[(Node, LinePos), Type]

  def genLLVM(n: Node)(implicit m: NodeMap): Seq[String] = {
    gen(n, 0)
  }

  private def gen(n: Node, id: Int)(implicit m: NodeMap): Seq[String] = n match {
    case n: ast.Integer => gen(n, id)
    case n: ast.Boolean => gen(n, id)
    case n: ast.NamedId => gen(n, id)
    case n: ast.Let     => gen(n, id)
    case n: ast.Mut     => gen(n, id)
    case n: ast.Fun     => gen(n, id)
    case n: ast.Top     => gen(n, id)
    case n: ast.App     => gen(n, id)
    case n: ast.Static  => gen(n, id)
    case n: ast.BinOp   => gen(n, id)
    case n: ast.UnaOp   => gen(n, id)
    case n: ast.Nop     => gen(n, id)
    case others         => ???
  }

  private def gen(n: ast.Integer)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Boolean)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.NamedId)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Static)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.BinOp)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.UnaOp)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Let)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Mut)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Fun)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Top)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Nop)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.App)(implicit m: NodeMap): Seq[String] = ???
}
