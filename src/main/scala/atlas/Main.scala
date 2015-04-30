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

object Main {
  // Command Line Outputs
  lazy val tooManyArguments = "atlas: error: too many arguments"
  lazy val usage = "Usage: atlas [src-file]"

  // Built-in Compiler Features.
  lazy val buildInTps = Set("Unit", "Int", "Boolean")
  lazy val builtInFns =
   Map(buildFnSym("println", Seq("Int")) -> types.Var("Unit"),
       buildFnSym("println", Seq("Boolean")) -> types.Var("Unit"))

  def main(args: Array[String]): Unit = {

    debugCompiler(verbose = false)
    // processCmd(args)
  }

  private def debugCompiler(verbose: Boolean): Unit = try {
    val stream = Source.fromURL(getClass().getResource("/atom.atlas"))
    val source = stream.mkString
    val tokens = mkTokens(source)

    if (verbose) {
      println("Tokens:")
      println(tokens.map(toString(_)).mkString("\n"))
    }

    val astree = mkASTree(tokens)
    val petree = partEval(astree)

    if (verbose) {
      println("ASTree:")
      println(petree)
    }

    val context = Context(buildInTps, builtInFns)
    val nodeMap = collectTypes(context, petree)
    val genCode = genLLVM(petree)(nodeMap)
    val genString = genCode.mkString("\n")

    if (verbose) {
      println("LLVM Output:")
      println(genString)
    }

    val path = "./bin/main"
    val output = new BufferedWriter(new FileWriter(new File(path + ".ll")))
    output.write(genString)
    output.close()
    (buildLLC(path + ".ll", path)   #&&
     buildLinker(path + ".o", path) #&&
     path).!
  }
  catch {
    case err: ParserError =>
      println(s"[error]${err.getMessage}")
    case err: TypeError =>
      println(s"[error]${err.getMessage}")
    case err: CodeGenError =>
      println(s"[error]${err.getMessage}")
  }

  private def processCmd(args: Seq[String]): Unit = {
     // Process Command Line Arguments.
    val srcPath = args match {
      case Seq(src) =>
        src
      case Seq() =>
        println(usage)
        return ()
      case other =>
        println(tooManyArguments)
        return ()
    }

    try {
      val filePath = new File(srcPath)
      val stream = Source.fromFile(filePath)
      val source = stream.mkString
      val tokens = mkTokens(source)
      val astree = mkASTree(tokens)
      val petree = partEval(astree)
      val context = Context(buildInTps, builtInFns)
      val nodeMap = collectTypes(context, petree)
      val genCode = genLLVM(petree)(nodeMap)
      val genString = genCode.mkString("\n")
      val prefix = filePath.getPath.split('.').head
      val fullnm = prefix + ".ll"
      val output = new BufferedWriter(new FileWriter(new File(fullnm)))

      output.write(genString)
      output.close()
      (buildLLC(fullnm, prefix) #&& buildLinker(prefix + ".o", prefix))

    }
    catch {
      case err: ParserError =>
        println(s"[error]${err.getMessage}")
      case err: TypeError =>
        println(s"[error]${err.getMessage}")
      case err: CodeGenError =>
        println(s"[error]${err.getMessage}")
    }
  }

  private def buildFnSym(name: String, params: Seq[String]): Symbol = {
    val pos = LinePos(1, 1)
    Symbol("", name, s"""(${params.mkString(", ")})""")(pos, true, true, 1)
  }

  private def buildLLC(src: String, dst: String): String =
    s"llc -filetype=obj $src -o $dst.o"

  private def buildLinker(src: String, dst: String): String = {
    val arch = System.getProperty("os.arch")
    val osVer = System.getProperty("os.version")
    val osxCmdOpt = s"macosx_version_min $osVer"
    s"ld -arch $arch -$osxCmdOpt -o $dst $src -lSystem"
  }

  private def toString(t: Token): String = t match {
    case _: tokens.NewLine => s"${t.pos}: \\n"
    case _                 => s"${t.pos}: ${t.raw}"
  }
}
