package atlas

import atlas.Lexer.mkTokens
import atlas.Parser.mkASTree
import atlas.TypeSystem.collectTypes
import atlas.PartialEvaluator.partEval
import atlas.CodeGen.genLLVM
import atlas.tokens.Token
import scala.io.Source
import scala.sys.process._
import java.io.{File, FileWriter, BufferedWriter}

object Main extends App {
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
    val builtin = Map(
      Symbol("", "println", "(Int)")(LinePos(1, 1), true, true, 1)
        -> types.Var("Unit"))
    val context = Context(prelude, builtin)
    val nodeMap = collectTypes(context, petree)

    val genCode = genLLVM(petree)(nodeMap)
    val genString = genCode.mkString("\n")
    println("LLVM IR:")
    println(genString)

    val outputFile = new File("bin/main.ll")
    val output = new BufferedWriter(new FileWriter(outputFile))
    output.write(genString)
    output.close()

    println("LLVM Output: ")
    ("llc -filetype=obj bin/main.ll -o bin/main.o" #&&
     buildLinkerCommand() #&&
     "./bin/main"
    ).!
  }
  catch {
    case err: ParserError =>
      println(s"[error]${err.getMessage}")
    case err: TypeError =>
      println(s"[error]${err.getMessage}")
    case err: CodeGenError =>
      println(s"[error]${err.getMessage}")
  }

  private def buildLinkerCommand(): String = {
    val arch = System.getProperty("os.arch")
    val osVer = System.getProperty("os.version")
    val osxCmdOpt = s"macosx_version_min $osVer"
    s"ld -arch $arch -$osxCmdOpt -o bin/main bin/main.o -lSystem"
  }

  private def toString(t: Token): String = t match {
    case _: tokens.NewLine => s"${t.pos}: \\n"
    case _                 => s"${t.pos}: ${t.raw}"
  }
}
