package atlas

case class LinePos(row: Int, col: Int) {
  override def toString = f"[$row%2d, $col%2d]"
}
