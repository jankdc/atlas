package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object CodeGen {
  type Store = Map[String, Int]
  type LiveHeap = Set[String]
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
      Seq[String](), Seq[Int](), e.id, Set[String]()){

      case ((ss, ids, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id))
        (ss ++ s, ids :+ newId, newId + 1, hh ++ h)
    }

    val calls = "call void"
    val alloc = s"%$id1 = alloca $tpStr, align 8"
    val initg = s"$calls @_Z11vector_initP6Vector($tpStr* %$id1)"
    val appGn = ids.map(
      id => s"$calls @_Z13vector_appendP6Vectori($tpStr* %$id1, $vtpStr %$id)")


    (argGen ++ Seq(alloc, initg) ++ appGn, id1, heap1 ++ Set(id1.toString))
   }

  private def gen(n: ast.Subscript, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val NodeMeta(tp, Some(sym)) = m.get(n)
    val arrTp = types.List(tp).toLLVMType
    val indTp = tp.toLLVMType
    val (indexGen, id1, heap1) = gen(n.arg, e)
    val id2 = id1 + 1
    val nm = {
      val actual = e.store.get(n.name) getOrElse n.name
      val prefix = if (sym.isStatic) s"@${sym.scope}" else "%"
      prefix + actual
    }

    val call = s"%$id2 = call $indTp @_Z10vector_getP6Vectori($arrTp* $nm, $indTp %$id1)"
    (indexGen :+ call, id2, heap1)
  }

  private def gen(n: ast.NamedId, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val id0 = e.id
    val atlas.NodeMeta(typeId, Some(sym)) = m.get(n)
    val tp = typeId.toLLVMTypeAlloc
    val nm = getStoreName(e.store, sym)

    (Seq(s"%$id0 = load $tp* $nm"), id0, Set())
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

  private def genFreeMemStruct(id: String): String = {
    s"call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
  }

  private def gen(n: ast.Mut, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) =
    (Seq(), e.id - 1, Set())

  private def gen(n: ast.Fun, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val Some(Symbol(sc0, nm0, ts0)) = m.get(n).sym
    val hashedTs0 = ts0.hashCode
    val id = e.id
    val tp = m.get(n.ret).typeid.toLLVMType
    val ag = n.params.map(m.get(_)).map(_.typeid.toLLVMType)
    val ns = n.params.map(p => s"%${p.name}${p.pos.row}${p.pos.col}")
    val ps = (ag, ns).zipped.toList.map(_.productIterator.toList.mkString(" "))
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
        val newId = id + 1
        val lta = m.get(n).typeid.toLLVMTypeAlloc
        val ins = s"  %$id = alloca $lta"
        (ss :+ ins, newId)
    }

    val psLen: Int = n.params.length
    val init = id1 - psLen
    val psIds = (init to psLen).toList

    val psStore = (n.params, psIds).zipped.toList.map {
      case (n@ast.Param(nm, _), id) =>
        val name = s"${nm}${n.pos.row}${n.pos.col}"
        val lta = m.get(n).typeid.toLLVMTypeAlloc
        s"  store $lta %$name, $lta* %$id"
    }

    val psMap = (n.params.map(
      p => s"${p.name}${p.pos.row}${p.pos.col}"
      ), psIds).zipped.toMap

    val (lhsGen, _) = lhs.foldLeft(Seq[String](), 0) {
      case ((ss, id), nn) =>
        val (g, newId, _) = gen(nn, e)
        (ss ++ g, newId)
    }

    val (rhsGen, id2, heap1) = rhs.foldLeft(
      Seq[String](),
      id1 - 1,
      Set[String]()) {
      case ((ss, id, hh), nn) =>
        val (g, newId, h) = gen(nn, Env(id + 1, psMap))
        (ss ++ g.map("  " ++ _), newId, hh ++ h)
    }

    val (retRes, id3) = {
      val (retGn, finalId) = tp match {
        case "void" =>
          (Seq(), id2)
        case "%struct.Vector" =>
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
      case "%struct.Vector" =>
        ((heap1 - id3.toString).map(genFreeMemStruct), Set[String](id3.toString))
      case primitives =>
        ((heap1).map(genFreeMemStruct), Set[String]())
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
    strConst += """@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1"""
    strConst += """@.str1 = private unnamed_addr constant [46 x i8] c"Index %d out of bounds for vector of size %d\0A\00", align 1"""
    strConst += """@.str-true = private unnamed_addr constant [6 x i8] c"true\0A\00", align 1"""
    strConst += """@.str-false = private unnamed_addr constant [7 x i8] c"false\0A\00", align 1"""

    val cinterface = mutable.Buffer[String]()
    cinterface += """declare i32 @printf(i8*, ...)"""
    cinterface += """declare i8* @malloc(i64)"""
    cinterface += """declare i8* @realloc(i8* nocapture, i64)"""
    cinterface += """declare void @exit(i32)"""
    cinterface += """declare void @free(i8* nocapture)"""
    cinterface += """declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1) #1"""

    val printlnCode = mutable.Buffer[String]()
    printlnCode += s"""define void @_println${"(Int)".hashCode}(i32 %n) {"""
    printlnCode += "entry:"
    printlnCode += s"  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %n)"
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

    val vectorCode = mutable.Buffer[String]()
    vectorCode += """; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z11vector_initP6Vector(%struct.Vector* nocapture %vector) {"""
    vectorCode += """  %1 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 0"""
    vectorCode += """  store i32 0, i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %2 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 1"""
    vectorCode += """  store i32 100, i32* %2, align 4, !tbaa !7"""
    vectorCode += """  %3 = tail call i8* @malloc(i64 400)"""
    vectorCode += """  %4 = bitcast i8* %3 to i32*"""
    vectorCode += """  %5 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 2"""
    vectorCode += """  store i32* %4, i32** %5, align 8, !tbaa !8"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
    vectorCode += """"""
    vectorCode += """; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z13vector_appendP6Vectori(%struct.Vector* nocapture %vector, i32 %value) {"""
    vectorCode += """  tail call void @_Z30vector_double_capacity_if_fullP6Vector(%struct.Vector* %vector)"""
    vectorCode += """  %1 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 0"""
    vectorCode += """  %2 = load i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %3 = add nsw i32 %2, 1"""
    vectorCode += """  store i32 %3, i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %4 = sext i32 %2 to i64"""
    vectorCode += """  %5 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 2"""
    vectorCode += """  %6 = load i32** %5, align 8, !tbaa !8"""
    vectorCode += """  %7 = getelementptr inbounds i32* %6, i64 %4"""
    vectorCode += """  store i32 %value, i32* %7, align 4, !tbaa !9"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
    vectorCode += """"""
    vectorCode += """; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z10vector_setP6Vectorii(%struct.Vector* nocapture %vector, i32 %index, i32 %value) {"""
    vectorCode += """  %1 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 0"""
    vectorCode += """  %2 = load i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %3 = icmp sgt i32 %2, %index"""
    vectorCode += """  br i1 %3, label %._crit_edge, label %.lr.ph"""
    vectorCode += """"""
    vectorCode += """.lr.ph:                                           ; preds = %0, %.lr.ph"""
    vectorCode += """  tail call void @_Z13vector_appendP6Vectori(%struct.Vector* %vector, i32 0)"""
    vectorCode += """  %4 = load i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %5 = icmp sgt i32 %4, %index"""
    vectorCode += """  br i1 %5, label %._crit_edge, label %.lr.ph"""
    vectorCode += """"""
    vectorCode += """._crit_edge:                                      ; preds = %.lr.ph, %0"""
    vectorCode += """  %6 = sext i32 %index to i64"""
    vectorCode += """  %7 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 2"""
    vectorCode += """  %8 = load i32** %7, align 8, !tbaa !8"""
    vectorCode += """  %9 = getelementptr inbounds i32* %8, i64 %6"""
    vectorCode += """  store i32 %value, i32* %9, align 4, !tbaa !9"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
    vectorCode += """; Function Attrs: ssp uwtable"""
    vectorCode += """define i32 @_Z10vector_getP6Vectori(%struct.Vector* nocapture readonly %vector, i32 %index) {"""
    vectorCode += """  %1 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 0"""
    vectorCode += """  %2 = load i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %3 = icmp sle i32 %2, %index"""
    vectorCode += """  %4 = icmp slt i32 %index, 0"""
    vectorCode += """  %or.cond = or i1 %3, %4"""
    vectorCode += """  br i1 %or.cond, label %5, label %7"""
    vectorCode += """"""
    vectorCode += """; <label>:5                                       ; preds = %0"""
    vectorCode += """  %6 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([46 x i8]* @.str1, i64 0, i64 0), i32 %index, i32 %2)"""
    vectorCode += """  tail call void @exit(i32 1) #4"""
    vectorCode += """  unreachable"""
    vectorCode += """"""
    vectorCode += """; <label>:7                                       ; preds = %0"""
    vectorCode += """  %8 = sext i32 %index to i64"""
    vectorCode += """  %9 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 2"""
    vectorCode += """  %10 = load i32** %9, align 8, !tbaa !8"""
    vectorCode += """  %11 = getelementptr inbounds i32* %10, i64 %8"""
    vectorCode += """  %12 = load i32* %11, align 4, !tbaa !9"""
    vectorCode += """  ret i32 %12"""
    vectorCode += """}"""
    vectorCode += """"""
    vectorCode += """; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z11vector_freeP6Vector(%struct.Vector* nocapture readonly %vector){"""
    vectorCode += """  %1 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 2"""
    vectorCode += """  %2 = load i32** %1, align 8"""
    vectorCode += """  %3 = bitcast i32* %2 to i8*"""
    vectorCode += """  tail call void @free(i8* %3)"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
    vectorCode += """; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z30vector_double_capacity_if_fullP6Vector(%struct.Vector* nocapture %vector) #1 {"""
    vectorCode += """  %1 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 0"""
    vectorCode += """  %2 = load i32* %1, align 4, !tbaa !1"""
    vectorCode += """  %3 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 1"""
    vectorCode += """  %4 = load i32* %3, align 4, !tbaa !7"""
    vectorCode += """  %5 = icmp slt i32 %2, %4"""
    vectorCode += """  br i1 %5, label %15, label %6"""
    vectorCode += """"""
    vectorCode += """; <label>:6                                       ; preds = %0"""
    vectorCode += """  %7 = shl nsw i32 %4, 1"""
    vectorCode += """  store i32 %7, i32* %3, align 4, !tbaa !7"""
    vectorCode += """  %8 = getelementptr inbounds %struct.Vector* %vector, i64 0, i32 2"""
    vectorCode += """  %9 = load i32** %8, align 8, !tbaa !8"""
    vectorCode += """  %10 = bitcast i32* %9 to i8*"""
    vectorCode += """  %11 = sext i32 %7 to i64"""
    vectorCode += """  %12 = shl nsw i64 %11, 2"""
    vectorCode += """  %13 = tail call i8* @realloc(i8* %10, i64 %12)"""
    vectorCode += """  %14 = bitcast i8* %13 to i32*"""
    vectorCode += """  store i32* %14, i32** %8, align 8, !tbaa !8"""
    vectorCode += """  br label %15"""
    vectorCode += """"""
    vectorCode += """; <label>:15                                      ; preds = %0, %6"""
    vectorCode += """  ret void"""
    vectorCode += """}"""

    val metaData = mutable.Buffer[String]()
    metaData += """!llvm.ident = !{!0}"""
    metaData += """!0 = metadata !{metadata !"Apple LLVM version 6.1.0 (clang-602.0.49) (based on LLVM 3.6.0svn)"}"""
    metaData += """!1 = metadata !{metadata !2, metadata !3, i64 0}"""
    metaData += """!2 = metadata !{metadata !"_ZTS6Vector", metadata !3, i64 0, metadata !3, i64 4, metadata !6, i64 8}"""
    metaData += """!3 = metadata !{metadata !"int", metadata !4, i64 0}"""
    metaData += """!4 = metadata !{metadata !"omnipotent char", metadata !5, i64 0}"""
    metaData += """!5 = metadata !{metadata !"Simple C/C++ TBAA"}"""
    metaData += """!6 = metadata !{metadata !"any pointer", metadata !4, i64 0}"""
    metaData += """!7 = metadata !{metadata !2, metadata !3, i64 4}"""
    metaData += """!8 = metadata !{metadata !2, metadata !6, i64 8}"""
    metaData += """!9 = metadata !{metadata !3, metadata !3, i64 0}"""

    val genRes =
      Seq(targetLayout, targetTriple) ++
      dataTypes.toSeq                 ++
      strConst.toSeq                  ++
      topGens                         ++
      mainEntry.toSeq                 ++
      printlnCode.toSeq               ++
      vectorCode.toSeq                ++
      cinterface.toSeq                ++
      metaData.toSeq

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
      Set[String]()) {
      case ((ss, dd, id, hh), arg) =>
        val (s, newId, h) = gen(arg, e.copy(id = id + 1))
        (ss ++ s, dd :+ newId, newId, hh ++ h)
    }

    val argTps = n.args.map(m.get(_).typeid.toLLVMType)
    val argIns = (argTps, dd)
     .zipped
     .toList
     .map { case (argTp, id) => s"$argTp %$id" }
     .mkString(", ")

    val (callGen, id2) =
    if (tp == "void") {
      val call = s"call $tp @_$sc$nm${ts.hashCode}($argIns)"
      (ps :+ call, id1)
    } else {
      val call = s"%${id1 + 1} = call $tp @_$sc$nm${ts.hashCode}($argIns)"
      (ps :+ call, id1 + 1)
    }

    typeId match {
      case types.List(_) =>
        val (allocGen, id3, heap2) = genAllocStore(id2 + 1, s"%$id2", typeId)
        (callGen ++ allocGen, id3, heap2 ++ heap1)
      case _ =>
        (callGen, id2, heap1)
    }
  }

  private def genAllocStore(id: Int, v: String, tp: Type)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val alloc = s"%$id = alloca ${tp.toLLVMType}"
    val store = s"store ${tp.toLLVMType} $v, ${tp.toLLVMType}* %$id"
    val heap1 =
      if (tp.isInstanceOf[types.List])
        Set[String](id.toString)
      else
        Set[String]()

    (Seq(alloc, store), id, heap1)
  }

  private def gen(n: ast.Cond, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tp = m.get(n).typeid.toLLVMType
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
      Set[String]()) {
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
      Set[String]()) {
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
          case id => s"  call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
        }
      case others =>
        ((heap1 ++ heap2 ++ heap3) - id4.toString).map {
          case id => s"  call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
        }
    }

    val stillNeedHeap = tp match {
      case "%struct.Vector" => Set[String](id4.toString)
      case others => Set[String]()
    }

    (Seq(alloc) ++
     (condGen :+ condBr) ++
     (bodyGen :+ bodyStore) ++
     (frees.toSeq :+ s"br label %$id3") ++
     (newIns.flatten :+ newRes), id4, stillNeedHeap)
   }

  private def gen(n: ast.Elif, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tp = m.get(n).typeid.toLLVMType
    val (condGen, id1, heap1) = gen(n.cond, e)
    val bodyId = id1 + 1
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      bodyId,
      Set[String]()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }
    var elseId = id2 + 1
    var condBr = s"br i1 %$id1, label %$bodyId, label %$elseId"

    val frees = tp match {
      case "void" =>
        (heap1 ++ heap2).map {
          case id => s"  call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
        }
      case others =>
        ((heap1 ++ heap2) - id2.toString).map {
          case id => s"  call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
        }
    }

    val stillNeedHeap = tp match {
      case "%struct.Vector" => Set[String](id2.toString)
      case others => Set[String]()
    }

    ((condGen :+ condBr) ++ bodyGen ++ frees, id2, stillNeedHeap)
   }

  private def gen(n: ast.Else, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, LiveHeap) = {
    val tp = m.get(n).typeid.toLLVMType
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      e.id - 1,
      Set[String]()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }

    val frees = tp match {
      case "void" =>
        (heap2).map {
          case id => s"  call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
        }
      case others =>
        ((heap2) - id2.toString).map {
          case id => s"  call void @_Z11vector_freeP6Vector(%struct.Vector* %$id)"
        }
    }

    val stillNeedHeap = tp match {
      case "%struct.Vector" => Set[String](id2.toString)
      case others => Set[String]()
    }

    (bodyGen ++ frees, id2, stillNeedHeap)
   }

  private implicit class LLVMTypeConverter(val t: Type) extends AnyVal {
    def toLLVMType = t match {
      case types.Var("Int")     => "i32"
      case types.Var("Boolean") => "i1"
      case types.Var("Unit")    => "void"
      case types.List(types.Var("Int")) => "%struct.Vector"
      case _ => ""
    }

    def toLLVMTypeAlloc = t match {
      case types.Var("Int")     => "i32"
      case types.Var("Boolean") => "i1"
      case types.Var("Unit")    => "{}"
      case types.List(types.Var("Int")) => "%struct.Vector"
      case _ => ""
    }
  }
}
