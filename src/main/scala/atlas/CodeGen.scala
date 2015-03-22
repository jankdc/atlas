package atlas

import atlas.ast.Node
import atlas.types.Type

case class GenEnv(arc: Map[(Node, LinePos), Type], acc: Int)

object CodeGen {
  def genLLVM(e: GenEnv, s: String, n: Node): Seq[String] = gen(e, "", n)

  private def gen(e: GenEnv, s: String, n: Node): Seq[String] = n match {
    case n: ast.Integer => gen(e, s, n)
    case n: ast.NamedId => gen(e, s, n)
    case n: ast.Let     => gen(e, s, n)
    case n: ast.Mut     => gen(e, s, n)
    case n: ast.Fun     => gen(e, s, n)
    case n: ast.Top     => gen(e, s, n)
    case n: ast.App     => gen(e, s, n)
    case n: ast.Static  => gen(e, s, n)
    case n: ast.BinOp   => gen(e, s, n)
    case n: ast.UnaOp   => gen(e, s, n)
    case n: ast.Nop     => gen(e, s, n)
    case others         => ???
  }

  private def gen(e: GenEnv, s: String, n: ast.Integer): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.NamedId): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.Static): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.BinOp): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.UnaOp): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.Let): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.Mut): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.Fun): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.Top): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.Nop): Seq[String] = ???

  private def gen(e: GenEnv, s: String, n: ast.App): Seq[String] = ???
}