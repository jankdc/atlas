package atlas

import atlas.tokens.Token
import scala.io.Source

object Main extends App {
  try {
    val path = "/atom.atlas"
    val source = Source.fromURL(getClass.getResource(path)).mkString
    println(s"Location: $path")

    val tokens = lex(source)
    println(s"Tokens:")
    println(tokens.map(toString(_)).mkString("\n"))

    val astRoot = parse(tokens)
    println("ASTree:")
    println(astRoot)

    val prelude = Set("Unit", "Int")
    val context = Context(prelude, Map())
    val topType = findType(Env(Map(), context), astRoot)
  }
  catch {
    case err: ParseError =>
      println(s"[error]${err.getMessage}")
    case err: CheckError =>
      println(s"[error]${err.getMessage}")
  }

  private def toString(t: Token): String = t match {
    case _: tokens.NewLine => s"${t.pos}: \\n"
    case _                 => s"${t.pos}: ${t.raw}"
  }
}
