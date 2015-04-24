package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object CodeGen {
  def genLLVM(n: Node)
   (implicit m: NodeMap): Seq[String] = gen(n, 1)

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
   (implicit m: NodeMap): Seq[String] = {
    val alloc = s"%$id = alloca i32, align 4"
    val store = s"store i32 ${n.value}, i32* %$id"
    val id1 = id + 1
    val load = s"%$id1 = load i32* %id, align 4"
    Seq(alloc, store, load)
  }

  private def gen(n: ast.Boolean, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    val alloc = s"%$id = alloca i1, align 4"
    val store = s"store i1 ${n.value}, i1* %$id"
    val id1 = id + 1
    val load = s"%$id1 = load i32* %id, align 4"
    Seq(alloc, store, load)
  }

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

    Seq(s"@$sc0$nm0 = internal constant $tp $data")
  }

  private def gen(n: ast.BinOp, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.UnaOp, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.Let, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    val tp = m.get(n.value).typeid.toLLVMTypeAlloc
    val alloc = s"%${n.name} = alloca $tp"
    Seq(alloc)
   }

  private def gen(n: ast.Mut, id: Int)
   (implicit m: NodeMap): Seq[String] =
    Seq()

  private def gen(n: ast.Fun, id: Int)
   (implicit m: NodeMap): Seq[String] = {
    val Some(Symbol(sc0, nm0, ts0)) = m.get(n).sym
    val hashedTs0 = ts0.hashCode
    val tp = m.get(n.ret).typeid.toLLVMType
    val ag = n.params.map(m.get(_)).map(_.typeid.toLLVMType)
    val ns = n.params.map("%" + _.name)
    val ps = (ag, ns).zipped.toList.map(_.productIterator.toList.mkString(" "))
    val res = ps.mkString(", ")
    val beg = s"define internal $tp @_$sc0$nm0$hashedTs0($res) {"
    val end = "}"
    val (lhs, rhs) = n.body.partition {
      case n: ast.Fun => true
      case n: ast.Static => true
      case _ => false
    }
    val lhsGen = lhs.map(gen(_, id)).flatten
    val rhsGen = rhs.map(gen(_, id)).flatten.map("  " ++ _)

    if (tp == "void") {
      return lhsGen ++ Seq(beg) ++ rhsGen ++ Seq(s"  ret void") ++ Seq(end)
    }
    else {
      lhsGen ++ Seq(beg) ++ rhsGen ++ Seq(s"  ret $tp %$id") ++ Seq(end)
    }
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
    mainEntry += s"  call void @_main${"()".hashCode}()"
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
      case types.Var("Unit")    => "void"
      case _ => ""
    }

    def toLLVMTypeAlloc = t match {
      case types.Var("Int")     => "i32"
      case types.Var("Boolean") => "i1"
      case types.Var("Unit")    => "{}"
      case _ => ""
    }
  }
}

