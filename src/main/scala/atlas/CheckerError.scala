package atlas

case class CheckerError(msg: String) extends RuntimeException(msg)
