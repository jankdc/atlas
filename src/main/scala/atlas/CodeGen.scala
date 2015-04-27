package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object CodeGen {
  type Store = Map[String, Int]
  case class Env(id: Int, store: Store)

  def genLLVM(n: Node)
   (implicit m: NodeMap): Seq[String] =
    gen(n, Env(1, Map())) match { case (s, _) => s }

  private def gen(n: Node, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = n match {
    case n: ast.Integer => gen(n, e)
    case n: ast.Boolean => gen(n, e)
    case n: ast.NamedId => gen(n, e)
    case n: ast.Let     => gen(n, e)
    case n: ast.Mut     => gen(n, e)
    case n: ast.Fun     => gen(n, e)
    case n: ast.Top     => gen(n, e)
    case n: ast.App     => gen(n, e)
    case n: ast.Static  => gen(n, e)
    case n: ast.BinOp   => gen(n, e)
    case n: ast.UnaOp   => gen(n, e)
    case n: ast.Nop     => gen(n, e)
    case others         => ???
  }

  private def gen(n: ast.Integer, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val id = e.id
    val alloc = s"%$id = alloca i32, align 4"
    val store = s"store i32 ${n.value}, i32* %$id"
    val id001 = id + 1
    val load = s"%$id001 = load i32* %$id, align 4"
    (Seq(alloc, store, load), id001)
  }

  private def gen(n: ast.Boolean, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val id = e.id
    val alloc = s"%$id = alloca i1, align 4"
    val store = s"store i1 ${n.value}, i1* %$id"
    val id001 = id + 1
    val load = s"%$id001 = load i1* %$id, align 4"
    (Seq(alloc, store, load), id001)
  }

  private def gen(n: ast.NamedId, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val atlas.NodeMeta(typeId, Some(sym)) = m.get(n)
    val tp = typeId.toLLVMTypeAlloc
    val id = e.id
    val nm = {
      val actual = e.store.get(n.name) getOrElse n.name
      val prefix = if (sym.isStatic) "@" else "%"
      prefix + actual
    }

    (Seq(s"%$id = load $tp* $nm"), id)
   }

  private def gen(n: ast.Static, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val Some(Symbol(sc0, nm0, _)) = m.get(n).sym
    val tp = m.get(n.value).typeid.toLLVMType
    val id = e.id

    val data = n.value match {
      case ast.Integer(n) => n
      case ast.Boolean(n) => n
      case _ =>
        val msg = s"${n.pos}: Static values must be constant expressions"
        throw CodeGenError(msg)
    }

    (Seq(s"@$sc0$nm0 = internal constant $tp $data"), id - 1)
  }

  private def gen(n: ast.BinOp, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val tp = m.get(n.lhs).typeid.toLLVMTypeAlloc

    val (lhs, id001) = gen(n.lhs, e)
    val (rhs, id002) = gen(n.rhs, e.copy(id = id001 + 1))

    val binOp = n.op match {
      case "==" => s"icmp eq $tp %${id002 - 1}, %${id002}"
      case "!=" => s"icmp ne $tp %${id002 - 1}, %${id002}"
      case "+" => s"add nsw $tp %${id002 - 1}, %${id002}"
      case "-" => s"sub nsw $tp %${id002 - 1}, %${id002}"
      case "*" => s"mul nsw $tp %${id002 - 1}, %${id002}"
      case "/" => s"sdiv $tp %${id002 - 1}, %${id002}"
      case _ => ???
    }

    val id003 = id002 + 1

    (lhs ++ rhs ++ Seq(s"%${id003} = $binOp"), id003)
  }


  private def gen(n: ast.UnaOp, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) =
    (Seq(), e.id - 1)

  private def gen(n: ast.Let, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val tp = m.get(n.value).typeid.toLLVMTypeAlloc
    val id = e.id
    val alloc = s"%${n.name} = alloca $tp"
    val (value, id001) = gen(n.value, e)
    val store = s"store $tp %$id001, $tp* %${n.name}"
    (value ++ Seq(alloc, store), id001)
   }

  private def gen(n: ast.Mut, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) =
    (Seq(), e.id - 1)

  private def gen(n: ast.Fun, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val Some(Symbol(sc0, nm0, ts0)) = m.get(n).sym
    val hashedTs0 = ts0.hashCode
    val id = e.id
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

    val (psAlloc, id001) = n.params.foldLeft(Seq[String](), 1) {
      case ((ss, id), n@ast.Param(nm, _)) =>
        val newId = id + 1
        val lta = m.get(n).typeid.toLLVMTypeAlloc
        val ins = s"  %$id = alloca $lta"
        (ss :+ ins, newId)
    }

    val psLen: Int = n.params.length
    val init = id001 - psLen
    val psIds = (init to psLen).toList

    val psStore = (n.params, psIds).zipped.toList.map {
      case (n@ast.Param(nm, _), id) =>
        val lta = m.get(n).typeid.toLLVMTypeAlloc
        s"  store $lta %$nm, $lta* %$id"
    }

    val psMap = (n.params.map(_.name), psIds).zipped.toMap

    val (lhsGen, _) = lhs.foldLeft(Seq[String](), 0) {
      case ((ss, id), nn) =>
        val (g, newId) = gen(nn, e)
        (ss ++ g, newId)
    }


    val (rhsGen, id002) = rhs.foldLeft(Seq[String](), id001 - 1) {
      case ((ss, id), nn) =>
        val (g, newId) = gen(nn, Env(id + 1, psMap))
        (ss ++ g.map("  " ++ _), newId)
    }

    val retRes = tp match {
      case "void" => s"$tp"
      case _ => s"$tp %$id002"
    }

    (lhsGen   ++
     Seq(beg) ++
     psAlloc  ++
     psStore  ++
     rhsGen   ++
     Seq(s"  ret $retRes", end), id002)
   }

  private def gen(n: ast.Top, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
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

    val printfInt = """@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1"""
    val printfBoolT = """@.str-true = private unnamed_addr constant [6 x i8] c"true\0A\00", align 1"""
    val printfBoolF = """@.str-false = private unnamed_addr constant [7 x i8] c"false\0A\00", align 1"""
    val printfDef = """declare i32 @printf(i8*, ...)"""

    val printlnInt = mutable.Buffer[String]()
    printlnInt += s"""define void @_println${"(Int)".hashCode}(i32 %n) {"""
    printlnInt += "entry:"
    printlnInt += s"  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %n)"
    printlnInt += s"  ret void"
    printlnInt += "}"

    val printlnBool = mutable.Buffer[String]()
    printlnBool += s"""define void @_println${"(Boolean)".hashCode}(i1 %n) {"""
    printlnBool += "entry:"
    printlnBool += "  br i1 %n, label %print-t, label %print-f"
    printlnBool += "print-t:"
    printlnBool += "  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str-true, i32 0, i32 0))"
    printlnBool += "  br label %join"
    printlnBool += ""
    printlnBool += "print-f:"
    printlnBool += "  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str-false, i32 0, i32 0))"
    printlnBool += "  br label %join"
    printlnBool += "join:"
    printlnBool += "  ret void"
    printlnBool += "}"

    val topGens = n.nodes.map(gen(_, e)).map(_._1).flatten

    val genRes =
      Seq(targetLayout, targetTriple) ++
      Seq(printfInt,
          printfBoolT,
          printfBoolF)                ++
      topGens                         ++
      mainEntry.toSeq                 ++
      printlnInt.toSeq                ++
      printlnBool.toSeq               ++
      Seq(printfDef)

    (genRes, e.id)
  }

  private def gen(n: ast.Nop, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) =
    (Seq(), e.id - 1)

  private def gen(n: ast.App, e: Env)
   (implicit m: NodeMap): (Seq[String], Int) = {
    val NodeMeta(typeId, Some(Symbol(_, nm, ts))) = m.get(n)
    val tp = typeId.toLLVMType
    val (ps, dd, id001) = n.args.foldLeft(Seq[String](), Seq[Int](), e.id - 1) {
      case ((ss, dd, id), arg) =>
        val (s, newId) = gen(arg, e.copy(id = id + 1))
        (ss ++ s, dd :+ newId, newId)
    }

    val argTps = n.args.map(m.get(_).typeid.toLLVMType)
    val argIns = (argTps, dd)
     .zipped
     .toList
     .map { case (argTp, id) => s"$argTp %$id" }
     .mkString(", ")

    if (tp == "void") {
      val call = s"call $tp @_$nm${ts.hashCode}($argIns)"
      (ps :+ call, id001)

    } else {
      val call = s"%${id001 + 1} = call $tp @_$nm${ts.hashCode}($argIns)"
      (ps :+ call, id001 + 1)
    }
   }

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
