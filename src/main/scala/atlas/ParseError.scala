package atlas

case class ParseError(count: Int, msg: String) extends RuntimeException(msg)

