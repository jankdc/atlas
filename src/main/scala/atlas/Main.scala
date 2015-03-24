package atlas

import atlas.Lexer.mkTokens
import atlas.Parser.mkASTree
import atlas.TypeSystem.collectTypes
import atlas.tokens.Token
import scala.io.Source

object Main extends App {
  try {
    val path = "/atom.atlas"
    val source = Source.fromURL(getClass.getResource(path)).mkString
    println(s"Location: $path")

    val tokens = mkTokens(source)
    println(s"Tokens:")
    println(tokens.map(toString(_)).mkString("\n"))

    val astree = mkASTree(tokens)
    println("ASTree:")
    println(astree)

    val prelude = Set("Unit", "Int", "Boolean")
    val context = Context(prelude, Map())
    val nodeMap = collectTypes(context, astree)
  }
  catch {
    case err: ParserError =>
      println(s"[error]${err.getMessage}")
    case err: TypeError =>
      println(s"[error]${err.getMessage}")
  }

  private def toString(t: Token): String = t match {
    case _: tokens.NewLine => s"${t.pos}: \\n"
    case _                 => s"${t.pos}: ${t.raw}"
  }
}
