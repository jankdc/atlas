package atlas
package types

sealed trait Type

case class Abs(terms: Seq[Type]) extends Type
case class Var(name: String) extends Type
