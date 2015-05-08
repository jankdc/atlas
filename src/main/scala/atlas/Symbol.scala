package atlas

case class Symbol(scope: String, name: String, opt: String = "")
(
  val pos: LinePos,
  val isStatic: Boolean,
  val isConstant: Boolean,
  val scopeLevel: Int,
  val returnsParam: Boolean = false
)
