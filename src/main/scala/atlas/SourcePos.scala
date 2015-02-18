package atlas

case class SourcePos(row: Int, column: Int) {
  override def toString(): String =
    f"[$row%2d, $column%2d]"
}
