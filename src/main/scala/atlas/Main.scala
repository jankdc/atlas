package atlas

import nodes.Node
import types.Type
import tokens.Token
import io.Source

object Main extends App {
  try {
    val path = "/atom.atlas"
    val stream = Source.fromURL(getClass.getResource(path))
    val source = stream.mkString
    val tokens = Lexer.lex(source)
    val astree = Parser.parse(tokens)
    val toptyp = Checker.check(astree)

    stream.close()

    println(s"Location: $path")
    println(s"Tokens:")
    tokens.foreach(printToken)
    println("\n\n")
    println(s"ASTree:\n$astree\n\n")

  }
  catch {
    case err: ParserError =>
      println(s"[error]${err.getMessage}")
    case err: CheckerError =>
      println(s"[error]${err.getMessage}")
  }

  private def printToken(token: Token): Unit = {
    print(s"${token.pos}: ")
    token match {
      case tokens.NewLine(n) =>
        println(s"\\n")
      case othertoken =>
        println(othertoken)
    }
  }
}
