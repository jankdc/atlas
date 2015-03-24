package atlas

import atlas.ast.Node
import atlas.types.Type

object CodeGen {
  def genLLVM(n: Node)(implicit m: NodeMap): Seq[String] = gen(n, 0)

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

  private def gen(n: ast.Integer, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Boolean, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.NamedId, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Static, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.BinOp, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.UnaOp, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Let, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Mut, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Fun, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Top, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.Nop, id: Int)(implicit m: NodeMap): Seq[String] = ???

  private def gen(n: ast.App, id: Int)(implicit m: NodeMap): Seq[String] = ???
}

