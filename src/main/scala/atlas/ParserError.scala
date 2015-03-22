package atlas

case class ParserError(count: Int, msg: String) extends RuntimeException(msg)

