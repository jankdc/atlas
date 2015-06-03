package atlas
package errors

abstract class AtlasError(msg: String) extends RuntimeException(msg)

case class ParsingError(count: Int, msg: String) extends AtlasError(msg)
case class TypingError(msg: String)              extends AtlasError(msg)
case class CodeGenError(msg: String)             extends AtlasError(msg)
case class MissingError(msg: String)             extends AtlasError(msg)
