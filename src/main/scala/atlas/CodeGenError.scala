package atlas

case class CodeGenError(msg: String) extends RuntimeException(msg)