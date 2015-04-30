package atlas
package types

sealed trait Type

case class Fun(terms: Seq[Type]) extends Type {
  override def toString = "(" + terms.mkString(", ") + ")"
}

case class Var(name: String) extends Type {
  override def toString = name
}

case class List(tp: Type) extends Type {
  override def toString = s"[$tp]"
}