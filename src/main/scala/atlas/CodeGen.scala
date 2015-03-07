package atlas

import nodes.Node

object CodeGen {
  def generate(n: Node): String = n match {
    case n: nodes.Integer => ???
    case n: nodes.NameId  => ???
    case n: nodes.Let     => ???
    case n: nodes.Mut     => ???
    case n: nodes.Fun     => ???
    case n: nodes.Top     => ???
    case n: nodes.App     => ???
    case n: nodes.Static  => generate(n)
    case n: nodes.BinOp   => ???
    case n: nodes.UnaOp   => ???
    case n: nodes.Nop     => ???
    case others           => ???
  }

  def generate(n: nodes.Static) = {

  }
}
