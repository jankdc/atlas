package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object CodeGen {
  def genLLVM(n: Node)
   (implicit m: NodeMap): Seq[String] = gen(n, 0)

  private def gen(n: Node, id: Int)
   (implicit m: NodeMap): Seq[String] = n match {
    case n: ast.Integer => gen(n, id)
    case n: ast.Boolean => gen(n, id)
    case n: ast.NamedId => gen(n, id)
    case n: ast.Let     => gen(n, id)
    case n: ast.Mut     => gen(n, id)
    case n: ast.Fun     => gen(n, id)
    case n: ast.Top     => gen(n, id)
    case n: ast.App     => gen(n, id)
    case n: ast.Static  => gen(n, id)
    case n: ast.BinOp   => gen(n, id)
    case n: ast.UnaOp   => gen(n, id)
    case n: ast.Nop     => gen(n, id)
    case others         => ???
  }

  private def gen(n: ast.Integer, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq(n.value.toString)

  private def gen(n: ast.Boolean, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq(n.value.toString)

  private def gen(n: ast.NamedId, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.Static, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    val Some(Symbol(sc0, nm0, _)) = m.get(n).sym
    val tp = m.get(n.value).typeid.toLLVMType

    val data = n.value match {
      case ast.Integer(n) => n
      case ast.Boolean(n) => n
      case _ =>
        val msg = s"${n.pos}: Static values must be constant expressions"
        throw CodeGenError(msg)
    }

    val sp = gen(n.value, id).mkString
    Seq(s"@$sc0$nm0 = internal constant $tp $sp")
  }

  private def gen(n: ast.BinOp, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.UnaOp, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.Let, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    Seq()
   }

  private def gen(n: ast.Mut, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.Fun, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    val Some(Symbol(sc0, nm0, _)) = m.get(n).sym
    val tp = m.get(n.ret).typeid.toLLVMType
    val ag = n.params.map(m.get(_)).map(_.typeid.toLLVMType)
    val ns = n.params.map("%" + _.name)
    val ps = (ag, ns).zipped.toList.map(_.productIterator.toList.mkString(" "))
    val res = ps.mkString(", ")
    val beg = s"define internal $tp @_$sc0$nm0($res) {"
    val end = "}"
    Seq(beg) ++ Seq(end)
   }

  private def gen(n: ast.Top, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    lazy val targetLayout = {
      """target datalayout = "E-S128-m:o-n8:16:32:64-f80:128-i64:64" """
    }

    lazy val targetTriple = {
      val osArch = System.getProperty("os.arch")
      val osName = System.getProperty("os.name").toLowerCase.filter(_ != ' ')
      val osVersion = System.getProperty("os.version")

      // TODO: This is only temporary because not all OS X is in Apple hardware
      // and not all linux are in PC hardware.
      val osVendor = osName match {
        case "macosx" => "apple"
        case "linux"  => "pc"
        case _        => "unknown"
      }

      s"""target triple = "$osArch-$osVendor-$osName$osVersion" """
    }


    // NOTE: When we're implementing a multi-file compiler,
    // this should be in another module to check if exactly only one
    // main function exists in the files.
    val mainAvailable = n.nodes.exists {
      case ast.Fun("main", Seq(), _, _) => true
      case _ => false
    }

    if (! mainAvailable) {
      throw CodeGenError(": No appropriate main function could be found.")
    }


    val mainEntry = mutable.Buffer[String]()
    mainEntry += "define i64 @main() {"
    mainEntry += "top:"
    mainEntry += "  call void @_main()"
    mainEntry += "  ret i64 0"
    mainEntry += "}"

    Seq(targetLayout, targetTriple) ++ n.nodes.map(gen(_, id)).flatten ++
      mainEntry.toSeq
  }

  private def gen(n: ast.Nop, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.App, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private implicit class LLVMTypeConverter(val t: Type) extends AnyVal {
    def toLLVMType = t match {
      case types.Var("Int")     => "i32"
      case types.Var("Boolean") => "i1"
      case _ => ""
    }
  }
}

