package atlas

import atlas.Lexer.mkTokens
import atlas.Parser.mkASTree
import atlas.TypeSystem.collectTypes
import atlas.PartialEvaluator.partEval
import atlas.CodeGen.genLLVM
import atlas.tokens.Token
import scala.io.Source
import scalax.io._

object Main extends App {
  implicit val codec = Codec.UTF8

  try {
    val path = "/atom.atlas"
    val source = Source.fromURL(getClass.getResource(path)).mkString
    println(s"Location: $path")

    val tokens = mkTokens(source)
    println(s"Tokens:")
    println(tokens.map(toString(_)).mkString("\n"))

    val astree = mkASTree(tokens)
    val petree = partEval(astree)
    println("ASTree:")
    println(petree)

    val prelude = Set("Unit", "Int", "Boolean")
    val context = Context(prelude, Map())
    val nodeMap = collectTypes(context, petree)

    val genCode = genLLVM(petree)(nodeMap)
    val genString = genCode.mkString("\n")
    println("LLVM IR:")
    println(genString)

    val output = Resource.fromFile("bin/main.ll")
    output.write(genString)
  }
  catch {
    case err: ParserError =>
      println(s"[error]${err.getMessage}")
    case err: TypeError =>
      println(s"[error]${err.getMessage}")
    case err: CodeGenError =>
      println(s"[error]${err.getMessage}")
  }

  private def toString(t: Token): String = t match {
    case _: tokens.NewLine => s"${t.pos}: \\n"
    case _                 => s"${t.pos}: ${t.raw}"
  }
}
