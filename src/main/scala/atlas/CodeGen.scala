package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object CodeGen {
  type Store = Map[String, Int]
  type LiveHeap = Set[(Type, String)]
  case class Env(id: Int, store: Store)

  def genLLVM(n: Node)
   (implicit m: NodeMap): Seq[String] =
    gen(n, Env(1, Map())) match { case (s, _, _) => s }

  private def gen(n: Node, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = n match {
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
    case n: ast.Cond    => gen(n, e)
    case n: ast.Elif    => gen(n, e)
    case n: ast.Else    => gen(n, e)
    case n: ast.Cons    => gen(n, e)
    case n: ast.Assign  => gen(n, e)
    case n: ast.Subscript => gen(n, e)
    case others         => ???
  }

  private def gen(n: ast.Integer, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val (gen, id0) = genAllocas(e.id, "i32", n.value.toString)
    (gen, id0, Set())
  }

  private def gen(n: ast.Boolean, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val (gen, id0) = genAllocas(e.id, "i1", n.value.toString)
    (gen, id0, Set())
  }

  private def gen(n: ast.Cons, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tp@types.List(vtp) = m.get(n).typeid
    val vtpStr = vtp.toLLVMType
    val tpStr = tp.toLLVMType

    val (argGen, ids, id1, heap1) = n.args.foldLeft(
      Seq[String](), Seq[Int](), e.id, Set[(Type, String)]()){

      case ((ss, ids, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id))
        (ss ++ s, ids :+ newId, newId + 1, hh ++ h)
    }

    val alloc = s"%$id1 = alloca $tpStr, align 8"

    val initSign = genCFnName("vector_init", Seq(tp), types.Var("Unit"))
    val initg = s"call $initSign($tpStr* %$id1)"

    val appSign = genCFnName("vector_append", Seq(tp, vtp), types.Var("Unit"))
    val appGn = ids.map(id => s"call $appSign($tpStr* %$id1, $vtpStr %$id)")

    (argGen ++ Seq(alloc, initg) ++ appGn, id1, heap1 ++ Set((tp, id1.toString)))
   }

  private def gen(n: ast.Subscript, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val NodeMeta(tp, Some(sym)) = m.get(n)
    val arrTp = types.List(tp).toLLVMType
    val argTp = m.get(n.arg).typeid
    val indTp = argTp.toLLVMType
    val (indexGen, id1, heap1) = gen(n.arg, e)
    val id2 = id1 + 1
    val nm = getStoreName(e.store, sym)

    val sign = genCFnName("vector_get", Seq(types.List(tp), argTp), tp)
    val call = s"%$id2 = call $sign($arrTp* $nm, $indTp %$id1)"
    (indexGen :+ call, id2, heap1)
  }

  private def gen(n: ast.NamedId, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val id0 = e.id
    val atlas.NodeMeta(typeId, Some(sym)) = m.get(n)
    val tp = typeId.toLLVMTypeAlloc
    val nm = getStoreName(e.store, sym)
    val ld = s"%$id0 = load $tp* $nm"


    typeId match {
      case types.List(_) =>
        val (gen, id1, heap1) = genAllocStore(id0 + 1, s"%$id0", typeId)
        (ld +: gen, id1, Set())
      case _ =>
        (Seq(ld), id0, Set())
    }
   }

  private def gen(n: ast.Assign, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val Some(sym) = m.get(n).sym
    val (valueGen, id1, heap1) = gen(n.value, e)
    val valTp = m.get(n.value).typeid.toLLVMType
    val nm = getStoreName(e.store, sym)
    val store = s"store $valTp %$id1, $valTp* $nm"

    (valueGen :+ store, id1, Set())
  }

  private def getStoreName(store: Store, sym: Symbol): String = {
    val name = s"${sym.name}${sym.pos.row}${sym.pos.col}"
    val actual = store.get(name) getOrElse name
    val prefix = if (sym.isStatic) s"@${sym.scope}" else "%"
    prefix + actual
  }

  private def gen(n: ast.Static, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val Some(sym@Symbol(sc0, nm0, _)) = m.get(n).sym
    val tp = m.get(n.value).typeid.toLLVMType
    val id0 = e.id - 1
    val name = s"${sym.name}${sym.pos.row}${sym.pos.col}"

    val data = n.value match {
      case ast.Integer(n) => n
      case ast.Boolean(n) => n
      case _ =>
        val msg = s"${n.pos}: Static values must be constant expressions"
        throw CodeGenError(msg)
    }

    (Seq(s"@$sc0$name = internal constant $tp $data"), id0 - 1, Set())
  }

  private def gen(n: ast.BinOp, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tp = m.get(n.lhs).typeid.toLLVMType

    val (lhs, id1, _) = gen(n.lhs, e)
    val (rhs, id2, _) = gen(n.rhs, e.copy(id = id1 + 1))

    val binOp = n.op match {
      case "=="  => s"icmp eq $tp %${id1}, %${id2}"
      case "!="  => s"icmp ne $tp %${id1}, %${id2}"
      case ">="  => s"icmp sge $tp %${id1}, %${id2}"
      case "<="  => s"icmp sle $tp %${id1}, %${id2}"
      case "<"   => s"icmp slt $tp %${id1}, %${id2}"
      case ">"   => s"icmp sgt $tp %${id1}, %${id2}"
      case "+"   => s"add nsw $tp %${id1}, %${id2}"
      case "-"   => s"sub nsw $tp %${id1}, %${id2}"
      case "*"   => s"mul nsw $tp %${id1}, %${id2}"
      case "/"   => s"sdiv $tp %${id1}, %${id2}"
      case "or"  => s"or $tp %${id1}, %${id2}"
      case "and" => s"and $tp %${id1}, %${id2}"
      case _ => ???
    }

    val id3 = id2 + 1
    (lhs ++ rhs ++ Seq(s"%${id3} = $binOp"), id3, Set())
  }


  private def gen(n: ast.UnaOp, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) =
    n.op match {
      case "-"  =>
        val binOp = ast.BinOp(ast.Integer(-1)(n.pos), "*", n.rhs)(n.pos)
        gen(binOp, e)
      case "!"  =>
        val (lhs, id1, _) = gen(n.rhs, e)
        (Seq(s"%${id1 + 1} = add i1 $id1, 1"), id1 + 1, Set())
      case _ => ???
    }

  private def gen(n: ast.Let, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tpId = m.get(n.value).typeid
    val tpSt = tpId.toLLVMType
    val name = s"${n.name}${n.pos.row}${n.pos.col}"

    val alloc = s"%$name = alloca $tpSt"
    val (valueGen1, id1, heap1) = gen(n.value, e)

    val (store, id3) = tpId match {
      case types.List(_) =>
        genMemCopy(id1 + 1, s"%$id1", s"%$name", tpSt)
      case primitiveType =>
        (Seq(s"store $tpSt %$id1, $tpSt* %$name"), id1)
    }

    (valueGen1 ++ Seq(alloc) ++ store, id3, heap1)
  }

  private def genAllocas(id: Int, tp: String, value: String)
    (implicit m: NodeMap): (Seq[String], Int) = {
    val id1 = id + 1

    val alloc = s"%$id = alloca $tp"
    val store = s"store $tp $value, $tp* %$id"
    val loads = s"%$id1 = load $tp* %$id"
    (Seq(alloc, store, loads), id1)
  }

  private def genMemCopy(id: Int, srcId: String, dstId: String, tp: String)
    (implicit m: NodeMap): (Seq[String], Int) = {
    val id1 = id
    val id2 = id1 + 1

    val srcCast = s"%$id1 = bitcast $tp* $srcId to i8*"
    val dstCast = s"%$id2 = bitcast $tp* $dstId to i8*"
    val memCopy = s"call void @llvm.memcpy.p0i8.p0i8.i64(i8* %$id2, i8* %$id1, i64 16, i32 8, i1 false)"

    (Seq(srcCast, dstCast, memCopy), id2)
  }

  private def genFreeMemStruct(id: String, tp: Type): String = {
    val sign = genCFnName("vector_free", Seq(tp), types.Var("Unit"))
    tp match {
      case types.List(_) => s"  call $sign(${tp.toLLVMType}* %$id)"
      case _ => s"  call $sign(%${tp.toLLVMType}* %$id)"
    }
  }

  private def gen(n: ast.Mut, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tpId = m.get(n.value).typeid
    val tpSt = tpId.toLLVMType
    val name = s"${n.name}${n.pos.row}${n.pos.col}"

    val alloc = s"%$name = alloca $tpSt"
    val (valueGen1, id1, heap1) = gen(n.value, e)

    val (store, id3) = tpId match {
      case types.List(_) =>
        genMemCopy(id1 + 1, s"%$id1", s"%$name", tpSt)
      case primitiveType =>
        (Seq(s"store $tpSt %$id1, $tpSt* %$name"), id1)
    }

    (valueGen1 ++ Seq(alloc) ++ store, id3, heap1)
  }

  private def gen(n: ast.Fun, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val Some(Symbol(sc0, nm0, ts0)) = m.get(n).sym
    val hashedTs0 = ts0.hashCode
    val id = e.id
    val fnTp = m.get(n.ret).typeid
    val tp = fnTp.toLLVMType
    val ag = n.params.map(m.get(_)).map(_.typeid)
    val ns = n.params.map(p => s"%${p.name}${p.pos.row}${p.pos.col}")
    val ps = (ag, ns).zipped.toList.map {
      case (s1: types.List, s2) => s"${s1.toLLVMType}* $s2"
      case (s1, s2) => s"${s1.toLLVMType} $s2"
    }

    val res = ps.mkString(", ")
    val beg = s"define internal $tp @_$sc0$nm0$hashedTs0($res) {"
    val end = "}"
    val retAlloc = if (tp != "void") Seq(s"  %.ret = alloca $tp") else Seq()

    val (lhs, rhs) = n.body.partition {
      case n: ast.Fun => true
      case n: ast.Static => true
      case _ => false
    }

    val (psAlloc, id1) = n.params.foldLeft(Seq[String](), 1) {
      case ((ss, id), n@ast.Param(nm, _)) =>
        val typeId = m.get(n).typeid
        val lta = typeId.toLLVMType

        val (ins, newId) = typeId match {
          case types.List(_) =>
            (s"", id)
          case _ =>
            (s"  %$id = alloca $lta", id + 1)
        }

        (ss :+ ins, newId)
    }

    val psLen: Int = ag.filterNot(_.isInstanceOf[types.List]).length
    val init = id1 - psLen
    val psIds = (init to psLen).toList

    val psStore = (n.params, psIds).zipped.toList.map {
      case (n@ast.Param(nm, _), id) =>
        val name = s"${nm}${n.pos.row}${n.pos.col}"
        val typeId = m.get(n).typeid
        val lta = typeId.toLLVMTypeAlloc

        typeId match {
          case types.List(_) =>
            ""
          case _ =>
            s"  store $lta %$name, $lta* %$id"
        }
    }

    val psMap = (n.params.filterNot {
        case p =>
          m.get(p).typeid match {
            case types.List(_) => true
            case _ => false
          }
    }.map(p => s"${p.name}${p.pos.row}${p.pos.col}"), psIds).zipped.toMap

    val (lhsGen, _) = lhs.foldLeft(Seq[String](), 0) {
      case ((ss, id), nn) =>
        val (g, newId, _) = gen(nn, e)
        (ss ++ g, newId)
    }

    val (rhsGen, id2, heap1) = rhs.foldLeft(
      Seq[String](),
      id1 - 1,
      Set[(Type, String)]()) {
      case ((ss, id, hh), nn) =>
        val (g, newId, h) = gen(nn, Env(id + 1, psMap))
        (ss ++ g.map("  " ++ _), newId, hh ++ h)
    }

    val (retRes, id3) = {
      val (retGn, finalId) = tp match {
        case "void" =>
          (Seq(), id2)
        case typeId if typeId.startsWith("%struct.") =>
          val idd1 = id2  + 1
          val idd2 = idd1 + 1
          val preamble1 = s"  %$idd1 = load $tp* %$id2"
          val preamble2 = s"  store $tp %$idd1, $tp* %.ret"
          val preamble3 = s"  %$idd2 = load $tp* %.ret"
          (Seq(preamble1, preamble2, preamble3), idd2)
        case others =>
          val id = id2 + 1
          val preamble1 = s"  store $tp %$id2, $tp* %.ret"
          val preamble2 = s"  %$id = load $tp* %.ret"
          (Seq(preamble1, preamble2), id)
      }

      val retTp = tp match {
        case "void" => s"  ret void"
        case _      => s"  ret $tp %$finalId"
      }

      (retGn :+ retTp, finalId)
    }

    val (frees, heap2) = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (fnTp, id2.toString)
        (
          (heap1 - retHeap).map { case (th, h) => genFreeMemStruct(h, th) },
          Set[(Type, String)](retHeap)
        )
      case primitives =>
        (
          (heap1).map { case (th, h) => genFreeMemStruct(h, th) },
          Set[(Type, String)]()
        )
    }

    (lhsGen   ++
     Seq(beg) ++
     retAlloc ++
     psAlloc  ++
     psStore  ++
     rhsGen   ++
     frees    ++
     retRes   ++
     Seq(end), id3, heap2)
   }

  private def genCFnName(n: String, args: Seq[Type], retv: Type): String = {
    val base = "@_Z"
    val nlen = n.length.toString
    val argsStr = args.map(_.toCType).mkString
    val retvStr = retv.toLLVMType
    s"$retvStr $base$nlen$n$argsStr"
  }

  private def gen(n: ast.Top, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
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

    val topGens = n.nodes.map(gen(_, e)).map(_._1).flatten


    val mainEntry = mutable.Buffer[String]()
    mainEntry += "define i64 @main() {"
    mainEntry += "top:"
    mainEntry += s"  call void @_main${"()".hashCode}()"
    mainEntry += "  ret i64 0"
    mainEntry += "}"

    val dataTypes = mutable.Buffer[String]()
    dataTypes += """%struct.Vector = type { i32, i32, i32* }"""

    val strConst = mutable.Buffer[String]()
    strConst += """@.str-num = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1"""
    strConst += """@.str-true = private unnamed_addr constant [6 x i8] c"true\0A\00", align 1"""
    strConst += """@.str-false = private unnamed_addr constant [7 x i8] c"false\0A\00", align 1"""

    val cinterface = mutable.Buffer[String]()
    cinterface += """declare i32 @printf(i8*, ...)"""
    cinterface += """declare i8* @malloc(i64)"""
    cinterface += """declare i8* @realloc(i8* nocapture, i64)"""
    cinterface += """declare void @exit(i32)"""
    cinterface += """declare void @free(i8* nocapture)"""
    cinterface += """declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1)"""

    val printlnCode = mutable.Buffer[String]()
    printlnCode += s"""define void @_println${"(Int)".hashCode}(i32 %n) {"""
    printlnCode += "entry:"
    printlnCode += s"  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str-num, i32 0, i32 0), i32 %n)"
    printlnCode += s"  ret void"
    printlnCode += "}"
    printlnCode += s"""define void @_println${"(Boolean)".hashCode}(i1 %n) {"""
    printlnCode += "entry:"
    printlnCode += "  br i1 %n, label %print-t, label %print-f"
    printlnCode += "print-t:"
    printlnCode += "  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str-true, i32 0, i32 0))"
    printlnCode += "  br label %join"
    printlnCode += ""
    printlnCode += "print-f:"
    printlnCode += "  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str-false, i32 0, i32 0))"
    printlnCode += "  br label %join"
    printlnCode += "join:"
    printlnCode += "  ret void"
    printlnCode += "}"

    val lenCode = mutable.Buffer[String]()
    lenCode += s"""define i32 @_len${"([Int])".hashCode}(%struct.VectorInt* %vector) {"""
    lenCode += "  %1 = call i32 @_Z3lenP9VectorInt(%struct.VectorInt* %vector)"
    lenCode += "  ret i32 %1"
    lenCode += "}"
    lenCode += s"""define i32 @_len${"([Boolean])".hashCode}(%struct.VectorBoolean* %vector) {"""
    lenCode += "  %1 = call i32 @_Z3lenP9VectorBoolean(%struct.VectorBoolean* %vector)"
    lenCode += "  ret i32 %1"
    lenCode += "}"

    val vectorCode = mutable.Buffer[String]()
    vectorCode += "%struct.VectorInt = type { i32, i32, i32* }"
    vectorCode += "%struct.VectorBoolean = type { i32, i32, i8* }"
    vectorCode += """@.str = private unnamed_addr constant [46 x i8] c"Index %d out of bounds for vector of size %d\0A\00", align 1"""
    vectorCode += """@.str1 = private unnamed_addr constant [2 x i8] c"[\00", align 1"""
    vectorCode += """@.str2 = private unnamed_addr constant [5 x i8] c"%d, \00", align 1"""
    vectorCode += """@.str3 = private unnamed_addr constant [3 x i8] c"%d\00", align 1"""
    vectorCode += """@.str4 = private unnamed_addr constant [3 x i8] c"]\0A\00", align 1"""
    vectorCode += """@.str5 = private unnamed_addr constant [7 x i8] c"true, \00", align 1"""
    vectorCode += """@.str6 = private unnamed_addr constant [8 x i8] c"false, \00", align 1"""
    vectorCode += """@.str7 = private unnamed_addr constant [5 x i8] c"true\00", align 1"""
    vectorCode += """@.str8 = private unnamed_addr constant [6 x i8] c"false\00", align 1"""
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z11vector_initP9VectorInt(%struct.VectorInt* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 0"
    vectorCode += "  store i32 0, i32* %3, align 4"
    vectorCode += "  %4 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %5 = getelementptr inbounds %struct.VectorInt* %4, i32 0, i32 1"
    vectorCode += "  store i32 100, i32* %5, align 4"
    vectorCode += "  %6 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %7 = getelementptr inbounds %struct.VectorInt* %6, i32 0, i32 1"
    vectorCode += "  %8 = load i32* %7, align 4"
    vectorCode += "  %9 = sext i32 %8 to i64"
    vectorCode += "  %10 = mul i64 4, %9"
    vectorCode += "  %11 = call i8* @malloc(i64 %10)"
    vectorCode += "  %12 = bitcast i8* %11 to i32*"
    vectorCode += "  %13 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %14 = getelementptr inbounds %struct.VectorInt* %13, i32 0, i32 2"
    vectorCode += "  store i32* %12, i32** %14, align 8"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z13vector_appendP9VectorInti(%struct.VectorInt* %vector, i32 %value) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  %2 = alloca i32, align 4"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  store i32 %value, i32* %2, align 4"
    vectorCode += "  %3 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  call void @_Z30vector_double_capacity_if_fullP9VectorInt(%struct.VectorInt* %3)"
    vectorCode += "  %4 = load i32* %2, align 4"
    vectorCode += "  %5 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %6 = getelementptr inbounds %struct.VectorInt* %5, i32 0, i32 0"
    vectorCode += "  %7 = load i32* %6, align 4"
    vectorCode += "  %8 = add nsw i32 %7, 1"
    vectorCode += "  store i32 %8, i32* %6, align 4"
    vectorCode += "  %9 = sext i32 %7 to i64"
    vectorCode += "  %10 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %11 = getelementptr inbounds %struct.VectorInt* %10, i32 0, i32 2"
    vectorCode += "  %12 = load i32** %11, align 8"
    vectorCode += "  %13 = getelementptr inbounds i32* %12, i64 %9"
    vectorCode += "  store i32 %4, i32* %13, align 4"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z30vector_double_capacity_if_fullP9VectorInt(%struct.VectorInt* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 0"
    vectorCode += "  %4 = load i32* %3, align 4"
    vectorCode += "  %5 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %6 = getelementptr inbounds %struct.VectorInt* %5, i32 0, i32 1"
    vectorCode += "  %7 = load i32* %6, align 4"
    vectorCode += "  %8 = icmp sge i32 %4, %7"
    vectorCode += "  br i1 %8, label %9, label %27"
    vectorCode += "; <label>:9                                       ; preds = %0"
    vectorCode += "  %10 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %11 = getelementptr inbounds %struct.VectorInt* %10, i32 0, i32 1"
    vectorCode += "  %12 = load i32* %11, align 4"
    vectorCode += "  %13 = mul nsw i32 %12, 2"
    vectorCode += "  store i32 %13, i32* %11, align 4"
    vectorCode += "  %14 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %15 = getelementptr inbounds %struct.VectorInt* %14, i32 0, i32 2"
    vectorCode += "  %16 = load i32** %15, align 8"
    vectorCode += "  %17 = bitcast i32* %16 to i8*"
    vectorCode += "  %18 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %19 = getelementptr inbounds %struct.VectorInt* %18, i32 0, i32 1"
    vectorCode += "  %20 = load i32* %19, align 4"
    vectorCode += "  %21 = sext i32 %20 to i64"
    vectorCode += "  %22 = mul i64 4, %21"
    vectorCode += "  %23 = call i8* @realloc(i8* %17, i64 %22)"
    vectorCode += "  %24 = bitcast i8* %23 to i32*"
    vectorCode += "  %25 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %26 = getelementptr inbounds %struct.VectorInt* %25, i32 0, i32 2"
    vectorCode += "  store i32* %24, i32** %26, align 8"
    vectorCode += "  br label %27"
    vectorCode += "; <label>:27                                      ; preds = %9, %0"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define i32 @_Z10vector_getP9VectorInti(%struct.VectorInt* %vector, i32 %index) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  %2 = alloca i32, align 4"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  store i32 %index, i32* %2, align 4"
    vectorCode += "  %3 = load i32* %2, align 4"
    vectorCode += "  %4 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %5 = getelementptr inbounds %struct.VectorInt* %4, i32 0, i32 0"
    vectorCode += "  %6 = load i32* %5, align 4"
    vectorCode += "  %7 = icmp sge i32 %3, %6"
    vectorCode += "  br i1 %7, label %11, label %8"
    vectorCode += "; <label>:8                                       ; preds = %0"
    vectorCode += "  %9 = load i32* %2, align 4"
    vectorCode += "  %10 = icmp slt i32 %9, 0"
    vectorCode += "  br i1 %10, label %11, label %17"
    vectorCode += "; <label>:11                                      ; preds = %8, %0"
    vectorCode += "  %12 = load i32* %2, align 4"
    vectorCode += "  %13 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %14 = getelementptr inbounds %struct.VectorInt* %13, i32 0, i32 0"
    vectorCode += "  %15 = load i32* %14, align 4"
    vectorCode += "  %16 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([46 x i8]* @.str, i32 0, i32 0), i32 %12, i32 %15)"
    vectorCode += "  call void @exit(i32 1) #4"
    vectorCode += "  unreachable"
    vectorCode += "; <label>:17                                      ; preds = %8"
    vectorCode += "  %18 = load i32* %2, align 4"
    vectorCode += "  %19 = sext i32 %18 to i64"
    vectorCode += "  %20 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %21 = getelementptr inbounds %struct.VectorInt* %20, i32 0, i32 2"
    vectorCode += "  %22 = load i32** %21, align 8"
    vectorCode += "  %23 = getelementptr inbounds i32* %22, i64 %19"
    vectorCode += "  %24 = load i32* %23, align 4"
    vectorCode += "  ret i32 %24"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z10vector_setP9VectorIntii(%struct.VectorInt* %vector, i32 %index, i32 %value) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  %2 = alloca i32, align 4"
    vectorCode += "  %3 = alloca i32, align 4"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  store i32 %index, i32* %2, align 4"
    vectorCode += "  store i32 %value, i32* %3, align 4"
    vectorCode += "  br label %4"
    vectorCode += "; <label>:4                                       ; preds = %10, %0"
    vectorCode += "  %5 = load i32* %2, align 4"
    vectorCode += "  %6 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %7 = getelementptr inbounds %struct.VectorInt* %6, i32 0, i32 0"
    vectorCode += "  %8 = load i32* %7, align 4"
    vectorCode += "  %9 = icmp sge i32 %5, %8"
    vectorCode += "  br i1 %9, label %10, label %12"
    vectorCode += "; <label>:10                                      ; preds = %4"
    vectorCode += "  %11 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  call void @_Z13vector_appendP9VectorInti(%struct.VectorInt* %11, i32 0)"
    vectorCode += "  br label %4"
    vectorCode += "; <label>:12                                      ; preds = %4"
    vectorCode += "  %13 = load i32* %3, align 4"
    vectorCode += "  %14 = load i32* %2, align 4"
    vectorCode += "  %15 = sext i32 %14 to i64"
    vectorCode += "  %16 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %17 = getelementptr inbounds %struct.VectorInt* %16, i32 0, i32 2"
    vectorCode += "  %18 = load i32** %17, align 8"
    vectorCode += "  %19 = getelementptr inbounds i32* %18, i64 %15"
    vectorCode += "  store i32 %13, i32* %19, align 4"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z11vector_freeP9VectorInt(%struct.VectorInt* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 2"
    vectorCode += "  %4 = load i32** %3, align 8"
    vectorCode += "  %5 = icmp ne i32* %4, null"
    vectorCode += "  br i1 %5, label %6, label %11"
    vectorCode += "  "
    vectorCode += "  ; <label>:6                                       ; preds = %0"
    vectorCode += "  %7 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %8 = getelementptr inbounds %struct.VectorInt* %7, i32 0, i32 2"
    vectorCode += "  %9 = load i32** %8, align 8"
    vectorCode += "  %10 = bitcast i32* %9 to i8*"
    vectorCode += "  call void @free(i8* %10)"
    vectorCode += "  br label %11"
    vectorCode += "  "
    vectorCode += "  ; <label>:11                                      ; preds = %6, %0"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_println-478497240(%struct.VectorInt* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  %i = alloca i32, align 4"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str1, i32 0, i32 0))"
    vectorCode += "  store i32 0, i32* %i, align 4"
    vectorCode += "  br label %3"
    vectorCode += "; <label>:3                                       ; preds = %19, %0"
    vectorCode += "  %4 = load i32* %i, align 4"
    vectorCode += "  %5 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %6 = getelementptr inbounds %struct.VectorInt* %5, i32 0, i32 0"
    vectorCode += "  %7 = load i32* %6, align 4"
    vectorCode += "  %8 = sub nsw i32 %7, 1"
    vectorCode += "  %9 = icmp slt i32 %4, %8"
    vectorCode += "  br i1 %9, label %10, label %22"
    vectorCode += "; <label>:10                                      ; preds = %3"
    vectorCode += "  %11 = load i32* %i, align 4"
    vectorCode += "  %12 = sext i32 %11 to i64"
    vectorCode += "  %13 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %14 = getelementptr inbounds %struct.VectorInt* %13, i32 0, i32 2"
    vectorCode += "  %15 = load i32** %14, align 8"
    vectorCode += "  %16 = getelementptr inbounds i32* %15, i64 %12"
    vectorCode += "  %17 = load i32* %16, align 4"
    vectorCode += "  %18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str2, i32 0, i32 0), i32 %17)"
    vectorCode += "  br label %19"
    vectorCode += "; <label>:19                                      ; preds = %10"
    vectorCode += "  %20 = load i32* %i, align 4"
    vectorCode += "  %21 = add nsw i32 %20, 1"
    vectorCode += "  store i32 %21, i32* %i, align 4"
    vectorCode += "  br label %3"
    vectorCode += "; <label>:22                                      ; preds = %3"
    vectorCode += "  %23 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %24 = getelementptr inbounds %struct.VectorInt* %23, i32 0, i32 0"
    vectorCode += "  %25 = load i32* %24, align 4"
    vectorCode += "  %26 = icmp ne i32 %25, 0"
    vectorCode += "  br i1 %26, label %27, label %39"
    vectorCode += "; <label>:27                                      ; preds = %22"
    vectorCode += "  %28 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %29 = getelementptr inbounds %struct.VectorInt* %28, i32 0, i32 0"
    vectorCode += "  %30 = load i32* %29, align 4"
    vectorCode += "  %31 = sub nsw i32 %30, 1"
    vectorCode += "  %32 = sext i32 %31 to i64"
    vectorCode += "  %33 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %34 = getelementptr inbounds %struct.VectorInt* %33, i32 0, i32 2"
    vectorCode += "  %35 = load i32** %34, align 8"
    vectorCode += "  %36 = getelementptr inbounds i32* %35, i64 %32"
    vectorCode += "  %37 = load i32* %36, align 4"
    vectorCode += "  %38 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str3, i32 0, i32 0), i32 %37)"
    vectorCode += "  br label %39"
    vectorCode += "; <label>:39                                      ; preds = %27, %22"
    vectorCode += "  %40 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str4, i32 0, i32 0))"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "define i32 @_Z3lenP9VectorInt(%struct.VectorInt* %vector) #0 {"
    vectorCode += "  %1 = alloca %struct.VectorInt*, align 8"
    vectorCode += "  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorInt** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 0"
    vectorCode += "  %4 = load i32* %3, align 4"
    vectorCode += "  ret i32 %4"
    vectorCode += "}"
    vectorCode += """; Function Attrs: ssp uwtable"""
    vectorCode += """define %struct.VectorInt @_Z11copy_vectorP9VectorInt(%struct.VectorInt* %vector) #1 {"""
    vectorCode += """  %1 = alloca %struct.VectorInt, align 8"""
    vectorCode += """  %2 = alloca %struct.VectorInt*, align 8"""
    vectorCode += """  %i = alloca i32, align 4"""
    vectorCode += """  %item = alloca i32, align 4"""
    vectorCode += """  store %struct.VectorInt* %vector, %struct.VectorInt** %2, align 8"""
    vectorCode += """  call void @_Z11vector_initP9VectorInt(%struct.VectorInt* %1)"""
    vectorCode += """  store i32 0, i32* %i, align 4"""
    vectorCode += """  br label %3"""
    vectorCode += """  """
    vectorCode += """  ; <label>:3                                       ; preds = %14, %0"""
    vectorCode += """  %4 = load i32* %i, align 4"""
    vectorCode += """  %5 = load %struct.VectorInt** %2, align 8"""
    vectorCode += """  %6 = getelementptr inbounds %struct.VectorInt* %5, i32 0, i32 0"""
    vectorCode += """  %7 = load i32* %6, align 4"""
    vectorCode += """  %8 = icmp slt i32 %4, %7"""
    vectorCode += """  br i1 %8, label %9, label %17"""
    vectorCode += """  """
    vectorCode += """  ; <label>:9                                       ; preds = %3"""
    vectorCode += """  %10 = load %struct.VectorInt** %2, align 8"""
    vectorCode += """  %11 = load i32* %i, align 4"""
    vectorCode += """  %12 = call i32 @_Z10vector_getP9VectorInti(%struct.VectorInt* %10, i32 %11)"""
    vectorCode += """  store i32 %12, i32* %item, align 4"""
    vectorCode += """  %13 = load i32* %item, align 4"""
    vectorCode += """  call void @_Z13vector_appendP9VectorInti(%struct.VectorInt* %1, i32 %13)"""
    vectorCode += """  br label %14"""
    vectorCode += """  """
    vectorCode += """  ; <label>:14                                      ; preds = %9"""
    vectorCode += """  %15 = load i32* %i, align 4"""
    vectorCode += """  %16 = add nsw i32 %15, 1"""
    vectorCode += """  store i32 %16, i32* %i, align 4"""
    vectorCode += """  br label %3"""
    vectorCode += """  """
    vectorCode += """  ; <label>:17                                      ; preds = %3"""
    vectorCode += """  %18 = load %struct.VectorInt* %1, align 8"""
    vectorCode += """  ret %struct.VectorInt %18"""
    vectorCode += """}"""
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z11vector_initP13VectorBoolean(%struct.VectorBoolean* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 0"
    vectorCode += "  store i32 0, i32* %3, align 4"
    vectorCode += "  %4 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %5 = getelementptr inbounds %struct.VectorBoolean* %4, i32 0, i32 1"
    vectorCode += "  store i32 100, i32* %5, align 4"
    vectorCode += "  %6 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %7 = getelementptr inbounds %struct.VectorBoolean* %6, i32 0, i32 1"
    vectorCode += "  %8 = load i32* %7, align 4"
    vectorCode += "  %9 = sext i32 %8 to i64"
    vectorCode += "  %10 = mul i64 1, %9"
    vectorCode += "  %11 = call i8* @malloc(i64 %10)"
    vectorCode += "  %12 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %13 = getelementptr inbounds %struct.VectorBoolean* %12, i32 0, i32 2"
    vectorCode += "  store i8* %11, i8** %13, align 8"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z13vector_appendP13VectorBooleanb(%struct.VectorBoolean* %vector, i1 zeroext %value) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  %2 = alloca i8, align 1"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %3 = zext i1 %value to i8"
    vectorCode += "  store i8 %3, i8* %2, align 1"
    vectorCode += "  %4 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  call void @_Z30vector_double_capacity_if_fullP13VectorBoolean(%struct.VectorBoolean* %4)"
    vectorCode += "  %5 = load i8* %2, align 1"
    vectorCode += "  %6 = trunc i8 %5 to i1"
    vectorCode += "  %7 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %8 = getelementptr inbounds %struct.VectorBoolean* %7, i32 0, i32 0"
    vectorCode += "  %9 = load i32* %8, align 4"
    vectorCode += "  %10 = add nsw i32 %9, 1"
    vectorCode += "  store i32 %10, i32* %8, align 4"
    vectorCode += "  %11 = sext i32 %9 to i64"
    vectorCode += "  %12 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %13 = getelementptr inbounds %struct.VectorBoolean* %12, i32 0, i32 2"
    vectorCode += "  %14 = load i8** %13, align 8"
    vectorCode += "  %15 = getelementptr inbounds i8* %14, i64 %11"
    vectorCode += "  %16 = zext i1 %6 to i8"
    vectorCode += "  store i8 %16, i8* %15, align 1"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z30vector_double_capacity_if_fullP13VectorBoolean(%struct.VectorBoolean* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 0"
    vectorCode += "  %4 = load i32* %3, align 4"
    vectorCode += "  %5 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %6 = getelementptr inbounds %struct.VectorBoolean* %5, i32 0, i32 1"
    vectorCode += "  %7 = load i32* %6, align 4"
    vectorCode += "  %8 = icmp sge i32 %4, %7"
    vectorCode += "  br i1 %8, label %9, label %25"
    vectorCode += "; <label>:9                                       ; preds = %0"
    vectorCode += "  %10 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %11 = getelementptr inbounds %struct.VectorBoolean* %10, i32 0, i32 1"
    vectorCode += "  %12 = load i32* %11, align 4"
    vectorCode += "  %13 = mul nsw i32 %12, 2"
    vectorCode += "  store i32 %13, i32* %11, align 4"
    vectorCode += "  %14 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %15 = getelementptr inbounds %struct.VectorBoolean* %14, i32 0, i32 2"
    vectorCode += "  %16 = load i8** %15, align 8"
    vectorCode += "  %17 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %18 = getelementptr inbounds %struct.VectorBoolean* %17, i32 0, i32 1"
    vectorCode += "  %19 = load i32* %18, align 4"
    vectorCode += "  %20 = sext i32 %19 to i64"
    vectorCode += "  %21 = mul i64 1, %20"
    vectorCode += "  %22 = call i8* @realloc(i8* %16, i64 %21)"
    vectorCode += "  %23 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %24 = getelementptr inbounds %struct.VectorBoolean* %23, i32 0, i32 2"
    vectorCode += "  store i8* %22, i8** %24, align 8"
    vectorCode += "  br label %25"
    vectorCode += "; <label>:25                                      ; preds = %9, %0"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define zeroext i1 @_Z10vector_getP13VectorBooleani(%struct.VectorBoolean* %vector, i32 %index) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  %2 = alloca i32, align 4"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  store i32 %index, i32* %2, align 4"
    vectorCode += "  %3 = load i32* %2, align 4"
    vectorCode += "  %4 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %5 = getelementptr inbounds %struct.VectorBoolean* %4, i32 0, i32 0"
    vectorCode += "  %6 = load i32* %5, align 4"
    vectorCode += "  %7 = icmp sge i32 %3, %6"
    vectorCode += "  br i1 %7, label %11, label %8"
    vectorCode += "; <label>:8                                       ; preds = %0"
    vectorCode += "  %9 = load i32* %2, align 4"
    vectorCode += "  %10 = icmp slt i32 %9, 0"
    vectorCode += "  br i1 %10, label %11, label %17"
    vectorCode += "; <label>:11                                      ; preds = %8, %0"
    vectorCode += "  %12 = load i32* %2, align 4"
    vectorCode += "  %13 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %14 = getelementptr inbounds %struct.VectorBoolean* %13, i32 0, i32 0"
    vectorCode += "  %15 = load i32* %14, align 4"
    vectorCode += "  %16 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([46 x i8]* @.str, i32 0, i32 0), i32 %12, i32 %15)"
    vectorCode += "  call void @exit(i32 1) #4"
    vectorCode += "  unreachable"
    vectorCode += "; <label>:17                                      ; preds = %8"
    vectorCode += "  %18 = load i32* %2, align 4"
    vectorCode += "  %19 = sext i32 %18 to i64"
    vectorCode += "  %20 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %21 = getelementptr inbounds %struct.VectorBoolean* %20, i32 0, i32 2"
    vectorCode += "  %22 = load i8** %21, align 8"
    vectorCode += "  %23 = getelementptr inbounds i8* %22, i64 %19"
    vectorCode += "  %24 = load i8* %23, align 1"
    vectorCode += "  %25 = trunc i8 %24 to i1"
    vectorCode += "  ret i1 %25"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z10vector_setP13VectorBooleanib(%struct.VectorBoolean* %vector, i32 %index, i1 zeroext %value) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  %2 = alloca i32, align 4"
    vectorCode += "  %3 = alloca i8, align 1"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  store i32 %index, i32* %2, align 4"
    vectorCode += "  %4 = zext i1 %value to i8"
    vectorCode += "  store i8 %4, i8* %3, align 1"
    vectorCode += "  br label %5"
    vectorCode += "; <label>:5                                       ; preds = %11, %0"
    vectorCode += "  %6 = load i32* %2, align 4"
    vectorCode += "  %7 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %8 = getelementptr inbounds %struct.VectorBoolean* %7, i32 0, i32 0"
    vectorCode += "  %9 = load i32* %8, align 4"
    vectorCode += "  %10 = icmp sge i32 %6, %9"
    vectorCode += "  br i1 %10, label %11, label %13"
    vectorCode += "; <label>:11                                      ; preds = %5"
    vectorCode += "  %12 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  call void @_Z13vector_appendP13VectorBooleanb(%struct.VectorBoolean* %12, i1 zeroext false)"
    vectorCode += "  br label %5"
    vectorCode += "; <label>:13                                      ; preds = %5"
    vectorCode += "  %14 = load i8* %3, align 1"
    vectorCode += "  %15 = trunc i8 %14 to i1"
    vectorCode += "  %16 = load i32* %2, align 4"
    vectorCode += "  %17 = sext i32 %16 to i64"
    vectorCode += "  %18 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %19 = getelementptr inbounds %struct.VectorBoolean* %18, i32 0, i32 2"
    vectorCode += "  %20 = load i8** %19, align 8"
    vectorCode += "  %21 = getelementptr inbounds i8* %20, i64 %17"
    vectorCode += "  %22 = zext i1 %15 to i8"
    vectorCode += "  store i8 %22, i8* %21, align 1"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_Z11vector_freeP13VectorBoolean(%struct.VectorBoolean* %vector) #1 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 2"
    vectorCode += "  %4 = load i8** %3"
    vectorCode += "  %5 = icmp ne i8* %4, null"
    vectorCode += "  br i1 %5, label %6, label %11"
    vectorCode += "  "
    vectorCode += "  ; <label>:6                                       ; preds = %0"
    vectorCode += "  %7 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %8 = getelementptr inbounds %struct.VectorBoolean* %7, i32 0, i32 2"
    vectorCode += "  %9 = load i8** %8"
    vectorCode += "  %10 = bitcast i8* %9 to i8*"
    vectorCode += "  call void @free(i8* %10)"
    vectorCode += "  br label %11"
    vectorCode += "  "
    vectorCode += "  ; <label>:11                                      ; preds = %6, %0"
    vectorCode += "  ret void"
    vectorCode += "}"
    vectorCode += "; Function Attrs: ssp uwtable"
    vectorCode += "define void @_println200954273(%struct.VectorBoolean* %vector) #1 {"
    vectorCode += """  %1 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  %i = alloca i32, align 4"""
    vectorCode += """  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([2 x i8]* @.str1, i32 0, i32 0))"""
    vectorCode += """  store i32 0, i32* %i, align 4"""
    vectorCode += """  br label %3"""
    vectorCode += """  """
    vectorCode += """  ; <label>:3                                       ; preds = %24, %0"""
    vectorCode += """  %4 = load i32* %i, align 4"""
    vectorCode += """  %5 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %6 = getelementptr inbounds %struct.VectorBoolean* %5, i32 0, i32 0"""
    vectorCode += """  %7 = load i32* %6, align 4"""
    vectorCode += """  %8 = sub nsw i32 %7, 1"""
    vectorCode += """  %9 = icmp slt i32 %4, %8"""
    vectorCode += """  br i1 %9, label %10, label %27"""
    vectorCode += """  """
    vectorCode += """  ; <label>:10                                      ; preds = %3"""
    vectorCode += """  %11 = load i32* %i, align 4"""
    vectorCode += """  %12 = sext i32 %11 to i64"""
    vectorCode += """  %13 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %14 = getelementptr inbounds %struct.VectorBoolean* %13, i32 0, i32 2"""
    vectorCode += """  %15 = load i8** %14, align 8"""
    vectorCode += """  %16 = getelementptr inbounds i8* %15, i64 %12"""
    vectorCode += """  %17 = load i8* %16, align 1"""
    vectorCode += """  %18 = trunc i8 %17 to i1"""
    vectorCode += """  br i1 %18, label %19, label %21"""
    vectorCode += """  """
    vectorCode += """  ; <label>:19                                      ; preds = %10"""
    vectorCode += """  %20 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([7 x i8]* @.str5, i32 0, i32 0))"""
    vectorCode += """  br label %23"""
    vectorCode += """  """
    vectorCode += """  ; <label>:21                                      ; preds = %10"""
    vectorCode += """  %22 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([8 x i8]* @.str6, i32 0, i32 0))"""
    vectorCode += """  br label %23"""
    vectorCode += """  """
    vectorCode += """  ; <label>:23                                      ; preds = %21, %19"""
    vectorCode += """  br label %24"""
    vectorCode += """  """
    vectorCode += """  ; <label>:24                                      ; preds = %23"""
    vectorCode += """  %25 = load i32* %i, align 4"""
    vectorCode += """  %26 = add nsw i32 %25, 1"""
    vectorCode += """  store i32 %26, i32* %i, align 4"""
    vectorCode += """  br label %3"""
    vectorCode += """  """
    vectorCode += """  ; <label>:27                                      ; preds = %3"""
    vectorCode += """  %28 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %29 = getelementptr inbounds %struct.VectorBoolean* %28, i32 0, i32 0"""
    vectorCode += """  %30 = load i32* %29, align 4"""
    vectorCode += """  %31 = icmp ne i32 %30, 0"""
    vectorCode += """  br i1 %31, label %32, label %49"""
    vectorCode += """  """
    vectorCode += """  ; <label>:32                                      ; preds = %27"""
    vectorCode += """  %33 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %34 = getelementptr inbounds %struct.VectorBoolean* %33, i32 0, i32 0"""
    vectorCode += """  %35 = load i32* %34, align 4"""
    vectorCode += """  %36 = sub nsw i32 %35, 1"""
    vectorCode += """  %37 = sext i32 %36 to i64"""
    vectorCode += """  %38 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %39 = getelementptr inbounds %struct.VectorBoolean* %38, i32 0, i32 2"""
    vectorCode += """  %40 = load i8** %39, align 8"""
    vectorCode += """  %41 = getelementptr inbounds i8* %40, i64 %37"""
    vectorCode += """  %42 = load i8* %41, align 1"""
    vectorCode += """  %43 = trunc i8 %42 to i1"""
    vectorCode += """  br i1 %43, label %44, label %46"""
    vectorCode += """  """
    vectorCode += """  ; <label>:44                                      ; preds = %32"""
    vectorCode += """  %45 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str7, i32 0, i32 0))"""
    vectorCode += """  br label %48"""
    vectorCode += """  """
    vectorCode += """  ; <label>:46                                      ; preds = %32"""
    vectorCode += """  %47 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([6 x i8]* @.str8, i32 0, i32 0))"""
    vectorCode += """  br label %48"""
    vectorCode += """  """
    vectorCode += """  ; <label>:48                                      ; preds = %46, %44"""
    vectorCode += """  br label %49"""
    vectorCode += """  """
    vectorCode += """  ; <label>:49                                      ; preds = %48, %27"""
    vectorCode += """  %50 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str4, i32 0, i32 0))"""
    vectorCode += """  ret void"""
    vectorCode += "}"
    vectorCode += "define i32 @_Z3lenP9VectorBoolean(%struct.VectorBoolean* %vector) #0 {"
    vectorCode += "  %1 = alloca %struct.VectorBoolean*, align 8"
    vectorCode += "  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %2 = load %struct.VectorBoolean** %1, align 8"
    vectorCode += "  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 0"
    vectorCode += "  %4 = load i32* %3, align 4"
    vectorCode += "  ret i32 %4"
    vectorCode += "}"
    vectorCode += """; Function Attrs: ssp uwtable"""
    vectorCode += """define %struct.VectorBoolean @_Z11copy_vectorP13VectorBoolean(%struct.VectorBoolean* %vector) #1 {"""
    vectorCode += """  %1 = alloca %struct.VectorBoolean, align 8"""
    vectorCode += """  %2 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  %i = alloca i32, align 4"""
    vectorCode += """  %item = alloca i32, align 4"""
    vectorCode += """  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %2, align 8"""
    vectorCode += """  call void @_Z11vector_initP13VectorBoolean(%struct.VectorBoolean* %1)"""
    vectorCode += """  store i32 0, i32* %i, align 4"""
    vectorCode += """  br label %3"""
    vectorCode += """  """
    vectorCode += """  ; <label>:3                                       ; preds = %16, %0"""
    vectorCode += """  %4 = load i32* %i, align 4"""
    vectorCode += """  %5 = load %struct.VectorBoolean** %2, align 8"""
    vectorCode += """  %6 = getelementptr inbounds %struct.VectorBoolean* %5, i32 0, i32 0"""
    vectorCode += """  %7 = load i32* %6, align 4"""
    vectorCode += """  %8 = icmp slt i32 %4, %7"""
    vectorCode += """  br i1 %8, label %9, label %19"""
    vectorCode += """  """
    vectorCode += """  ; <label>:9                                       ; preds = %3"""
    vectorCode += """  %10 = load %struct.VectorBoolean** %2, align 8"""
    vectorCode += """  %11 = load i32* %i, align 4"""
    vectorCode += """  %12 = call zeroext i1 @_Z10vector_getP13VectorBooleani(%struct.VectorBoolean* %10, i32 %11)"""
    vectorCode += """  %13 = zext i1 %12 to i32"""
    vectorCode += """  store i32 %13, i32* %item, align 4"""
    vectorCode += """  %14 = load i32* %item, align 4"""
    vectorCode += """  %15 = icmp ne i32 %14, 0"""
    vectorCode += """  call void @_Z13vector_appendP13VectorBooleanb(%struct.VectorBoolean* %1, i1 zeroext %15)"""
    vectorCode += """  br label %16"""
    vectorCode += """  """
    vectorCode += """  ; <label>:16                                      ; preds = %9"""
    vectorCode += """  %17 = load i32* %i, align 4"""
    vectorCode += """  %18 = add nsw i32 %17, 1"""
    vectorCode += """  store i32 %18, i32* %i, align 4"""
    vectorCode += """  br label %3"""
    vectorCode += """  """
    vectorCode += """  ; <label>:19                                      ; preds = %3"""
    vectorCode += """  %20 = load %struct.VectorBoolean* %1, align 8"""
    vectorCode += """  ret %struct.VectorBoolean %20"""
    vectorCode += """}"""
    vectorCode += """attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }"""
    vectorCode += """attributes #1 = { ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }"""
    vectorCode += """attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }"""
    vectorCode += """attributes #3 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }"""
    vectorCode += """attributes #4 = { noreturn }"""
    vectorCode += """!llvm.ident = !{!0}"""
    vectorCode += """!0 = metadata !{metadata !"Apple LLVM version 6.1.0 (clang-602.0.49) (based on LLVM 3.6.0svn)"}"""


    val genRes =
      Seq(targetLayout, targetTriple) ++
      dataTypes.toSeq                 ++
      strConst.toSeq                  ++
      topGens                         ++
      mainEntry.toSeq                 ++
      printlnCode.toSeq               ++
      lenCode.toSeq                   ++
      vectorCode.toSeq                ++
      cinterface.toSeq

    (genRes, e.id, Set())
  }

  private def gen(n: ast.Nop, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) =
    (Seq(), e.id - 1, Set())

  private def gen(n: ast.App, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val NodeMeta(typeId, Some(Symbol(sc, nm, ts))) = m.get(n)
    val tp = typeId.toLLVMType
    val (ps, dd, id1, heap1) = n.args.foldLeft(
      Seq[String](),
      Seq[Int](),
      e.id - 1,
      Set[(Type, String)]()) {
      case ((ss, dd, id, hh), arg) =>
        val (s, newId, h) = gen(arg, e.copy(id = id + 1))
        (ss ++ s, dd :+ newId, newId, hh ++ h)
    }

    val argTps = n.args.map(m.get(_).typeid.toLLVMType)
    val argIns = (argTps, dd)
     .zipped
     .toList
     .map {
        case (argTp, id) if argTp.startsWith("%struct.") => s"$argTp* %$id"
        case (argTp, id)    => s"$argTp %$id"
      }
     .mkString(", ")

    val (callGen, id2) =
      if (tp == "void") {
        val call = s"call $tp @_$sc$nm${ts.hashCode}($argIns)"
        (Seq(call), id1)
      }
      else {
        val call = s"%${id1 + 1} = call $tp @_$sc$nm${ts.hashCode}($argIns)"
        (Seq(call), id1 + 1)
      }

    typeId match {
      case types.List(_) =>
        val (allocGen, id3, heap2) = genAllocStore(id2 + 1, s"%$id2", typeId)
        (ps ++ callGen ++ allocGen, id3, heap2 ++ heap1)
      case _ =>
        (ps ++ callGen, id2, heap1)
    }
  }

  private def genAllocStore(id: Int, v: String, tp: Type)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val alloc = s"%$id = alloca ${tp.toLLVMType}"
    val store = s"store ${tp.toLLVMType} $v, ${tp.toLLVMType}* %$id"
    (Seq(alloc, store), id, Set())
  }

  private def gen(n: ast.Cond, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val (alloc, condId) =
      if (tp != "void")
        (s"%${e.id} = alloca $tp", e.id + 1)
      else
        ("", e.id)

    val (condGen, id1, heap1) = gen(n.cond, e.copy(id = condId))
    val bodyId = id1 + 1
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      bodyId,
      Set[(Type, String)]()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }
    val bodyStore =
      if (tp != "void")
        s"store $tp %$id2, $tp* %${e.id}"
      else
        ""

    var elseId = id2 + 1
    var condBr = s"br i1 %$id1, label %$bodyId, label %$elseId"

    val (g, id3, heap3) = n.others.foldLeft(
      Seq[Seq[String]](),
      elseId,
      Set[(Type, String)]()) {
      case ((ss, id, hh), n) =>
        // Needs to skip 2 instruction ids in order to provide space
        // for label ids.
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        val withStore =
          if (tp != "void")
            s :+ s"store $tp %$newId, $tp* %${e.id}"
          else
            s
        (ss :+ withStore, newId + 1, hh ++ h)
    }

    val newIns = g.map(_ :+ s"br label %$id3")
    val (newRes, id4) =
      if (tp != "void")
        (s"%${id3 + 1} = load $tp* %${e.id}", id3 + 1)
      else
       ("", id3)

    val frees = tp match {
      case "void" =>
        (heap1 ++ heap2 ++ heap3).map {
          case (th, h) => genFreeMemStruct(h, th)
        }
      case others =>
        val retHeap = (retTp, id4.toString)
        (heap1 ++ heap2 ++ heap3 - retHeap).map {
          case (th, h) => genFreeMemStruct(h, th)
        }
    }

    val stillNeedHeap = tp match {
      case struct if struct.startsWith("%struct.") =>
        Set[(Type, String)]((retTp, id4.toString))
      case others =>
        Set[(Type, String)]()
    }

    (Seq(alloc) ++
     (condGen :+ condBr) ++
     (bodyGen :+ bodyStore) ++
     (frees.toSeq :+ s"br label %$id3") ++
     (newIns.flatten :+ newRes), id4, stillNeedHeap)
   }

  private def gen(n: ast.Elif, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val (condGen, id1, heap1) = gen(n.cond, e)
    val bodyId = id1 + 1
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      bodyId,
      Set[(Type, String)]()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }
    var elseId = id2 + 1
    var condBr = s"br i1 %$id1, label %$bodyId, label %$elseId"

    val frees = tp match {
      case "void" =>
        (heap1 ++ heap2).map {
          case (th, h) => genFreeMemStruct(h, th)
        }
      case others =>
        val retHeap = (retTp, id2.toString)
        (heap1 ++ heap2 - retHeap).map {
          case (th, h) => genFreeMemStruct(h, th)
        }
    }

    val stillNeedHeap = tp match {
      case struct if struct.startsWith("%struct.") =>
        Set[(Type, String)]((retTp, id2.toString))
      case others =>
        Set[(Type, String)]()
    }

    ((condGen :+ condBr) ++ bodyGen ++ frees, id2, stillNeedHeap)
   }

  private def gen(n: ast.Else, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      e.id - 1,
      Set[(Type, String)]()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }

    val frees = tp match {
      case "void" =>
        (heap2).map {
          case (th, h) => genFreeMemStruct(h, th)
        }
      case others =>
        val retHeap = (retTp, id2.toString)
        (heap2 - retHeap).map {
          case (th, h) => genFreeMemStruct(h, th)
        }
    }

    val stillNeedHeap = tp match {
      case struct if struct.startsWith("%struct.") =>
        Set[(Type, String)]((retTp, id2.toString))
      case others =>
        Set[(Type, String)]()
    }

    (bodyGen ++ frees, id2, stillNeedHeap)
   }

  private implicit class LLVMTypeConverter(val t: Type) extends AnyVal {
    def toLLVMType = t match {
      case types.Var("Int")     => "i32"
      case types.Var("Boolean") => "i1"
      case types.Var("Unit")    => "void"
      case types.List(types.Var(s)) => s"%struct.Vector$s"
      case tp@types.List(types.List(_)) =>
        val msg = s": LLVM Code Generation for $tp is currently not supported yet! Please refer to the documentation."
        throw NotImplementedFeature(msg)
      case _ => ???
    }

    def toLLVMTypeAlloc = t match {
      case types.Var("Int")     => "i32"
      case types.Var("Boolean") => "i1"
      case types.Var("Unit")    => "{}"
      case types.List(types.Var(s)) => s"%struct.Vector$s"
      case tp@types.List(types.List(_)) =>
        val msg = s": LLVM Code Generation for $tp is currently not supported yet! Please refer to the documentation."
        throw NotImplementedFeature(msg)
      case _ => ???
    }

    def toCType = t match {
      case types.Var("Int")     => "i"
      case types.Var("Boolean") => "b"
      case types.Var("Unit")    => ""
      case types.List(types.Var(s)) =>
        val base = "Vector" + s
        val pref = "P" + base.length.toString
        pref + base
      case tp@types.List(types.List(_)) =>
        val msg = s": LLVM Code Generation for $tp is currently not supported yet! Please refer to the documentation."
        throw NotImplementedFeature(msg)
      case _ => ???
    }
  }
}
