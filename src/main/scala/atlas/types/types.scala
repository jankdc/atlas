package atlas
package types

sealed trait Type

case class Fun(terms: Seq[Type]) extends Type
case class App(name: String, ret: Type) extends Type
case class Var(name: String) extends Type