package atlas

case class TypeError(msg: String) extends RuntimeException(msg)
