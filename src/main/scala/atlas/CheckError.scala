package atlas

case class CheckError(msg: String) extends RuntimeException(msg)
