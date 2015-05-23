package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object CodeGen {
  type Store = Map[String, Int]
  private def Store(): Store = Map[String, Int]()

  type HeapItem = (Type, String)
  private def HeapItem(typeId: Type, heapId: Int): HeapItem =
    (typeId, heapId.toString)

  type HeapStore = Map[HeapItem, String]
  private def HeapStore(): HeapStore =
    Map[HeapItem, String]()

  case class Env(id: Int, store: Store, debugMode: Boolean)

  def genLLVM(n: Node, debugMode: Boolean = false)
   (implicit m: NodeMap): Seq[String] =
    gen(n, Env(1, Map(), debugMode)) match { case (s, _, _) => s }

  private def gen(n: Node, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = n match {
    case n: ast.Integer => gen(n, e)
    case n: ast.Boolean => gen(n, e)
    case n: ast.Identifier => gen(n, e)
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
    case n: ast.While   => gen(n, e)
    case n: ast.For     => gen(n, e)
    case n: ast.Subscript => gen(n, e)
    case n: ast.AssignSub => gen(n, e)
    case others         => ???
  }

  private def gen(n: ast.Integer, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val (gen, id0) = genAllocas(e.id, "i32", n.value.toString)
    (gen, id0, HeapStore())
  }

  private def gen(n: ast.Boolean, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val (gen, id0) = genAllocas(e.id, "i1", n.value.toString)
    (gen, id0, HeapStore())
  }


  private implicit class CodeGenHelper(val ns: Seq[Node]) extends AnyVal {
    def generate(env: Env)
     (implicit m: NodeMap): (Seq[String], Seq[Int], HeapStore) = {
      val (gs, is, hs, _) = ns.foldLeft(
         Seq[String](),
         Seq[Int](),
         HeapStore(),
         env) { case ((rs, is, hs, env), n) =>

          val (r, i, h) = gen(n, env)
          (rs ++ r, is :+ i, hs ++ h, env.copy(id = i + 1))
      }

      (gs, is, hs)
    }
  }

  private def gen(n: ast.Cons, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val tp@types.List(vtp) = m.get(n).typeid
    val vtpStr = vtp.toLLVMType
    val tpStr = tp.toLLVMType

    val (argGen, ids, heap1) = n.args.generate(e)
    val id1 = (ids.lastOption getOrElse e.id - 1) + 1
    val alloc = s"%$id1 = alloca $tpStr, align 8"
    val initSign = genCFnName("vector_init", Seq(tp), types.Var("Unit"))
    val initg = s"call $initSign($tpStr* %$id1)"
    val appSign = genCFnName("vector_append", Seq(tp, vtp), types.Var("Unit"))
    val appGn = ids.map(id => s"call $appSign($tpStr* %$id1, $vtpStr %$id)")

    val hash = s"(${tp.toString})".hashCode
    val printAlloc =
      if (e.debugMode) {
        Seq(s"  call void @__debugAlloc$hash($tpStr* %$id1)")
      } else {
        Seq("")
      }

    (argGen ++
     Seq(alloc) ++
     Seq(initg) ++
     appGn      ++
     printAlloc, id1, heap1 ++ Map((tp -> (id1).toString) -> (id1).toString))
   }

  private def gen(n: ast.Subscript, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
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

  private def gen(n: ast.Identifier, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val id0 = e.id
    val atlas.NodeMeta(typeId, Some(sym)) = m.get(n)
    val tp = typeId.toLLVMType
    val nm = getStoreName(e.store, sym)
    val ld = s"%$id0 = load $tp* $nm"


    typeId match {
      case types.List(_) =>
        val (gen, id1, heap1) = genAllocStore(id0 + 1, s"%$id0", typeId)
        (ld +: gen, id1, Map((typeId -> id1.toString) -> nm.tail))
      case _ =>
        (Seq(ld), id0, Map())
    }
   }

  private def gen(n: ast.Assign, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val NodeMeta(symTp, Some(sym)) = m.get(n)
    val (valueGen, id1, heap1) = gen(n.value, e)
    val valTp = m.get(n.value).typeid
    val tp = valTp.toLLVMType
    val nm = getStoreName(e.store, sym)

    val (extraGen, id2, heap2) = (n.op, valTp) match {
      case ("+=", _) =>
        val lstTp = types.List(valTp)
        val name = genCFnName("vector_append", Seq(lstTp, valTp), types.Var("Unit"))
        val stpId = s"${lstTp.toLLVMType}*"
        val vtpId = s"${valTp.toLLVMType}"
        val call = s"call $name($stpId $nm, $vtpId %$id1)"
        (Seq(call), id1, HeapStore())
      case (_, types.List(_)) =>
        val cname = genCFnName("copy_vector", Seq(valTp), valTp)
        val fname = genCFnName("vector_free", Seq(valTp), types.Var("Unit"))
        val free = s"call $fname($tp* $nm)"

        val temp = s"%${id1 + 1} = alloca $tp"
        val (tempCpy, d0) = genMemCopy(id1 + 2, nm, s"%${id1 + 1}", tp)

        val call = s"%${d0 + 1} = call $cname($tp* %$id1)"
        val (ge, d1, _) = genAllocStore(d0 + 2, s"%${d0 + 1}", valTp)
        val (gm, d2) = genMemCopy(d1 + 1, s"%$d1", nm, valTp.toLLVMType)
        (Seq(temp) ++ tempCpy ++ Seq(call, free) ++ ge ++ gm, d2, Map((valTp, d1.toString) -> nm.tail))
      case _ =>
        (Seq(s"store $tp %$id1, $tp* $nm"), id1, HeapStore())
    }

    (valueGen ++ extraGen, id2, heap1 ++ heap2)
  }

  private def gen(n: ast.AssignSub, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val NodeMeta(symTp, Some(sym)) = m.get(n)
    val (indexGen, id1, heap1) = gen(n.index, e)
    val (valueGen, id2, heap2) = gen(n.value, e.copy(id = id1 + 1))
    val indTp = m.get(n.index).typeid
    val valTp = m.get(n.value).typeid
    val tp = valTp.toLLVMType
    val nm = getStoreName(e.store, sym)

    val (extraGen, id3, heap3) = (indTp, n.op, valTp) match {
      case (_, "=", _) =>
        val lstTp = types.List(valTp)
        val name = genCFnName("vector_set", Seq(lstTp, indTp, valTp), types.Var("Unit"))
        val stpId = s"${lstTp.toLLVMType}*"
        val indId = s"${indTp.toLLVMType}"
        val vtpId = s"${valTp.toLLVMType}"
        val call = s"call $name($stpId $nm, $indId %$id1, $vtpId %$id2)"
        (Seq(call), id2, HeapStore())
      case (_, _, _) => ???
    }

    (indexGen ++ valueGen ++ extraGen, id3, heap1 ++ heap2 ++ heap3)
  }

  private def getStoreName(store: Store, sym: Symbol): String = {
    val name = s"${sym.name}${sym.pos.row}${sym.pos.col}"
    val actual = store.get(name) getOrElse name
    val prefix = if (sym.isStatic) s"@${sym.scope}" else "%"
    prefix + actual
  }

  private def gen(n: ast.Static, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
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

    (Seq(s"@$sc0$name = internal constant $tp $data"), id0 - 1, Map())
  }

  private def gen(n: ast.BinOp, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
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
      case "%"   => s"srem $tp %${id1}, %${id2}"
      case _ => ???
    }

    val id3 = id2 + 1
    (lhs ++ rhs ++ Seq(s"%${id3} = $binOp"), id3, Map())
  }


  private def gen(n: ast.UnaOp, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) =
    n.op match {
      case "-"  =>
        val binOp = ast.BinOp(ast.Integer(-1)(n.pos), "*", n.rhs)(n.pos)
        gen(binOp, e)
      case "!"  =>
        val (lhs, id1, _) = gen(n.rhs, e)
        (Seq(s"%${id1 + 1} = add i1 $id1, 1"), id1 + 1, Map())
      case _ => ???
    }

  private def gen(n: ast.Let, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val tpId = m.get(n.value).typeid
    val tpSt = tpId.toLLVMType
    val name = s"${n.name}${n.pos.row}${n.pos.col}"

    val alloc = s"%$name = alloca $tpSt"
    val (valueGen1, id1, heap1) = gen(n.value, e)

    val (store, id3, heap2) = (n.value, tpId) match {
      case (ast.Identifier(_), types.List(_)) =>
        val cname = genCFnName("copy_vector", Seq(tpId), tpId)
        val call = s"%${id1 + 1} = call $cname($tpSt* %$id1)"
        val sstore = s"store $tpSt %${id1 + 1}, $tpSt* %$name"
        (Seq(call, sstore), id1 + 1, Map((tpId, (id1 + 1).toString) -> name))
      case (_, types.List(_)) =>
        val (g, id) = genMemCopy(id1 + 1, s"%$id1", s"%$name", tpSt)
        val otherHeap = heap1.toSeq.init
        (g, id, Map((tpId, (id).toString) -> name) ++ otherHeap.toMap)
      case primitiveType =>
        (Seq(s"store $tpSt %$id1, $tpSt* %$name"), id1, HeapStore())
    }

    (valueGen1 ++ Seq(alloc) ++ store, id3, heap2)
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
    val memCopy = s"call void @llvm.memcpy.p0i8.p0i8.i64(i8* %$id2, i8* %$id1, i64 24, i32 8, i1 false)"

    (Seq(srcCast, dstCast, memCopy), id2)
  }

  private def genFreeMemStruct(e: Env, id: String, tp: Type): String = {
    val sign = genCFnName("vector_free", Seq(tp), types.Var("Unit"))
    val hash = s"(${tp.toString})".hashCode
    val tpId = tp.toLLVMType

    val printDlloc = if (e.debugMode) {
      Seq(s"  call void @__debugDlloc$hash($tpId* %$id)")
    } else {
      Seq("")
    }

    val free = tp match {
      case types.List(_) => s"  call $sign(${tp.toLLVMType}* %$id)"
      case _ => s"  call $sign(%${tp.toLLVMType}* %$id)"
    }

    (printDlloc :+ free).mkString("\n")
  }

  private def gen(n: ast.Mut, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val tpId = m.get(n.value).typeid
    val tpSt = tpId.toLLVMType
    val name = s"${n.name}${n.pos.row}${n.pos.col}"

    val alloc = s"%$name = alloca $tpSt"
    val (valueGen1, id1, heap1) = gen(n.value, e)

    val (store, id3, heap2) = (n.value, tpId) match {
      case (ast.Identifier(_), types.List(_)) =>
        val cname = genCFnName("copy_vector", Seq(tpId), tpId)
        val call = s"%${id1 + 1} = call $cname($tpSt* %$id1)"
        val sstore = s"store $tpSt %${id1 + 1}, $tpSt* %$name"
        (Seq(call, sstore), id1 + 1, Map((tpId, (id1 + 1).toString) -> name))
      case (_, types.List(_)) =>
        val (g, id) = genMemCopy(id1 + 1, s"%$id1", s"%$name", tpSt)
        val otherHeap = heap1.toSeq.init
        (g, id, Map((tpId, (id).toString) -> name) ++ otherHeap.toMap)
      case primitiveType =>
        (Seq(s"store $tpSt %$id1, $tpSt* %$name"), id1, HeapStore())
    }

    (valueGen1 ++ Seq(alloc) ++ store, id3, heap2)
  }

  private def gen(n: ast.Fun, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
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
        val lta = typeId.toLLVMType

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
      HeapStore()) {
      case ((ss, id, hh), nn) =>
        val (g, newId, h) = gen(nn, Env(id + 1, psMap, e.debugMode))
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

    val paramNames = n.params.map(p => s"${p.name}${p.pos.row}${p.pos.col}")

    val (frees, heap2) = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (fnTp, id2.toString)
        val allHeap = heap1
        val heapId = allHeap.get(retHeap) getOrElse ""
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, _), hid) => hid != heapId }
          .filterNot { case ((tp, _), hid) => paramNames contains hid }
          .map { case ((tp, _), hid) => (tp, hid) }


        (
          heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) },
          HeapStore()
        )
      case primitives =>
        val allHeap = heap1

        val heapSeq = allHeap.toSeq
          .filterNot { case ((tp, _), hid) => paramNames contains hid }
          .map { case ((tp, _), hid) => (tp, hid) }

        (
          heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) },
          HeapStore()
        )
    }

    val bodyRes = Seq(retAlloc, psAlloc, psStore, rhsGen, frees.toSet, retRes).flatten
    val nameRegex = " *\\%[a-zA-Z]".r
    val (namedAlloc, rest) = bodyRes.partition {
      case s => nameRegex.findPrefixOf(s).mkString.nonEmpty
    }
    val sortedBody = namedAlloc ++ rest
    val result = lhsGen ++ Seq(beg) ++ sortedBody ++ Seq(end)

    (result, id3, heap2)
   }

  private def genCFnName(n: String, args: Seq[Type], retv: Type): String = {
    val base = "@_Z"
    val nlen = n.length.toString
    val argsStr = args.map(_.toCType).mkString
    val retvStr = retv.toLLVMType
    s"$retvStr $base$nlen$n$argsStr"
  }

  private def gen(n: ast.Top, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
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
    strConst += """@.str-alloc = private unnamed_addr constant [27 x i8] c"New allocation of size %d\0A\00", align 1"""
    strConst += """@.str-dlloc = private unnamed_addr constant [26 x i8] c"De-allocation of size %d\0A\00", align 1"""

    val cinterface = mutable.Buffer[String]()
    cinterface += """declare i32 @printf(i8*, ...)"""
    cinterface += """declare i8* @malloc(i64)"""
    cinterface += """declare i8* @realloc(i8* nocapture, i64)"""
    cinterface += """declare void @exit(i32)"""
    cinterface += """declare void @free(i8* nocapture)"""
    cinterface += """declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1)"""

    val copyCode = mutable.Buffer[String]()
    copyCode += s"""define void @__copyI(%struct.VectorInt* %lhs, %struct.VectorInt* %rhs) {"""
    copyCode += """%1 = bitcast %struct.VectorInt* %lhs to i8*"""
    copyCode += """%2 = bitcast %struct.VectorInt* %rhs to i8*"""
    copyCode += """call void @llvm.memcpy.p0i8.p0i8.i64(i8* %2, i8* %1, i64 24, i32 8, i1 false)"""
    copyCode += "ret void"
    copyCode += """}"""

    copyCode += s"""define void @__copyB(%struct.VectorBoolean* %lhs, %struct.VectorBoolean* %rhs) {"""
    copyCode += """%1 = bitcast %struct.VectorBoolean* %lhs to i8*"""
    copyCode += """%2 = bitcast %struct.VectorBoolean* %rhs to i8*"""
    copyCode += """call void @llvm.memcpy.p0i8.p0i8.i64(i8* %2, i8* %1, i64 24, i32 8, i1 false)"""
    copyCode += "ret void"
    copyCode += """}"""

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

    val debugCode = mutable.Buffer[String]()
    debugCode += s"""define void @__debugAlloc${"([Int])".hashCode}(%struct.VectorInt* %vector) {"""
    debugCode += "entry:"
    debugCode += s"""  %0 = call i32 @_len${"([Int])".hashCode}(%struct.VectorInt* %vector)"""
    debugCode += s"""  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([27 x i8]* @.str-alloc, i32 0, i32 0), i32 %0)"""
    debugCode += s"""  ret void"""
    debugCode += "}"
    debugCode += s"""define void @__debugDlloc${"([Int])".hashCode}(%struct.VectorInt* %vector) {"""
    debugCode += "entry:"
    debugCode += s"""  %0 = call i32 @_len${"([Int])".hashCode}(%struct.VectorInt* %vector)"""
    debugCode += s"""  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([26 x i8]* @.str-dlloc, i32 0, i32 0), i32 %0)"""
    debugCode += s"""  ret void"""
    debugCode += "}"
    debugCode += s"""define void @__debugAlloc${"([Boolean])".hashCode}(%struct.VectorBoolean* %vector) {"""
    debugCode += "entry:"
    debugCode += s"""  %0 = call i32 @_len${"([Boolean])".hashCode}(%struct.VectorBoolean* %vector)"""
    debugCode += s"""  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([27 x i8]* @.str-alloc, i32 0, i32 0), i32 %0)"""
    debugCode += s"""  ret void"""
    debugCode += "}"
    debugCode += s"""define void @__debugDlloc${"([Boolean])".hashCode}(%struct.VectorBoolean* %vector) {"""
    debugCode += "entry:"
    debugCode += s"""  %0 = call i32 @_len${"([Boolean])".hashCode}(%struct.VectorBoolean* %vector)"""
    debugCode += s"""  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([26 x i8]* @.str-dlloc, i32 0, i32 0), i32 %0)"""
    debugCode += s"""  ret void"""
    debugCode += "}"

    val lenCode = mutable.Buffer[String]()
    lenCode += s"""define i32 @_len${"([Int])".hashCode}(%struct.VectorInt* %vector) {"""
    lenCode += "  %1 = call i32 @_Z3lenP9VectorInt(%struct.VectorInt* %vector)"
    lenCode += "  ret i32 %1"
    lenCode += "}"
    lenCode += s"""define i32 @_len${"([Boolean])".hashCode}(%struct.VectorBoolean* %vector) {"""
    lenCode += "  %1 = call i32 @_Z3lenP13VectorBoolean(%struct.VectorBoolean* %vector)"
    lenCode += "  ret i32 %1"
    lenCode += "}"

    val vectorCode = mutable.Buffer[String]()
    vectorCode += """%struct.VectorBoolean = type { i32, i32, i8*, i8 }"""
    vectorCode += """%struct.VectorInt = type { i32, i32, i32*, i8 }"""
    vectorCode += """@.VectorBoolean = private unnamed_addr constant { i32, i32, i8*, i8, [7 x i8] } { i32 0, i32 0, i8* null, i8 0, [7 x i8] undef }, align 8"""
    vectorCode += """@.VectorInt = private unnamed_addr constant { i32, i32, i32*, i8, [7 x i8] } { i32 0, i32 0, i32* null, i8 0, [7 x i8] undef }, align 8"""
    vectorCode += """@.str = private unnamed_addr constant [46 x i8] c"Index %d out of bounds for vector of size %d\0A\00", align 1"""
    vectorCode += """@.str1 = private unnamed_addr constant [2 x i8] c"[\00", align 1"""
    vectorCode += """@.str2 = private unnamed_addr constant [5 x i8] c"%d, \00", align 1"""
    vectorCode += """@.str3 = private unnamed_addr constant [3 x i8] c"%d\00", align 1"""
    vectorCode += """@.str4 = private unnamed_addr constant [3 x i8] c"]\0A\00", align 1"""
    vectorCode += """@.str5 = private unnamed_addr constant [7 x i8] c"true, \00", align 1"""
    vectorCode += """@.str6 = private unnamed_addr constant [8 x i8] c"false, \00", align 1"""
    vectorCode += """@.str7 = private unnamed_addr constant [5 x i8] c"true\00", align 1"""
    vectorCode += """@.str8 = private unnamed_addr constant [6 x i8] c"false\00", align 1"""
    vectorCode += """  ; Function Attrs: ssp uwtable"""
    vectorCode += """define void @_Z11vector_initP9VectorInt(%struct.VectorInt* %vector) #0 {"""
    vectorCode += """  %1 = alloca %struct.VectorInt*, align 8"""
    vectorCode += """  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 0"""
    vectorCode += """  store i32 0, i32* %3, align 4"""
    vectorCode += """  %4 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %5 = getelementptr inbounds %struct.VectorInt* %4, i32 0, i32 1"""
    vectorCode += """  store i32 100, i32* %5, align 4"""
    vectorCode += """  %6 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %7 = getelementptr inbounds %struct.VectorInt* %6, i32 0, i32 3"""
    vectorCode += """  store i8 0, i8* %7, align 1"""
    vectorCode += """  %8 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %9 = getelementptr inbounds %struct.VectorInt* %8, i32 0, i32 1"""
    vectorCode += """  %10 = load i32* %9, align 4"""
    vectorCode += """  %11 = sext i32 %10 to i64"""
    vectorCode += """  %12 = mul i64 4, %11"""
    vectorCode += """  %13 = call i8* @malloc(i64 %12)"""
    vectorCode += """  %14 = bitcast i8* %13 to i32*"""
    vectorCode += """  %15 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %16 = getelementptr inbounds %struct.VectorInt* %15, i32 0, i32 2"""
    vectorCode += """  store i32* %14, i32** %16, align 8"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
    vectorCode += """  ; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z19vector_set_borrowedP9VectorInt(%struct.VectorInt* %vector) #3 {"""
    vectorCode += """  %1 = alloca %struct.VectorInt*, align 8"""
    vectorCode += """  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 3"""
    vectorCode += """  store i8 1, i8* %3, align 1"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
    vectorCode += """  ; Function Attrs: ssp uwtable"""
    vectorCode += """define void @_Z11vector_freeP9VectorInt(%struct.VectorInt* %vector) #0 {"""
    vectorCode += """  %1 = alloca %struct.VectorInt*, align 8"""
    vectorCode += """  store %struct.VectorInt* %vector, %struct.VectorInt** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorInt* %2, i32 0, i32 2"""
    vectorCode += """  %4 = load i32** %3, align 8"""
    vectorCode += """  %5 = icmp ne i32* %4, null"""
    vectorCode += """  br i1 %5, label %6, label %16"""
    vectorCode += """  """
    vectorCode += """  ; <label>:6                                       ; preds = %0"""
    vectorCode += """  %7 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %8 = getelementptr inbounds %struct.VectorInt* %7, i32 0, i32 3"""
    vectorCode += """  %9 = load i8* %8, align 1"""
    vectorCode += """  %10 = trunc i8 %9 to i1"""
    vectorCode += """  br i1 %10, label %16, label %11"""
    vectorCode += """  """
    vectorCode += """  ; <label>:11                                      ; preds = %6"""
    vectorCode += """  %12 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %13 = getelementptr inbounds %struct.VectorInt* %12, i32 0, i32 2"""
    vectorCode += """  %14 = load i32** %13, align 8"""
    vectorCode += """  %15 = bitcast i32* %14 to i8*"""
    vectorCode += """  call void @free(i8* %15)"""
    vectorCode += """  br label %16"""
    vectorCode += """  """
    vectorCode += """  ; <label>:16                                      ; preds = %11, %6, %0"""
    vectorCode += """  %17 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %18 = getelementptr inbounds %struct.VectorInt* %17, i32 0, i32 2"""
    vectorCode += """  store i32* null, i32** %18, align 8"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
    vectorCode += """define void @_Z23vector_free_lhs_if_diffP9VectorIntP9VectorInt(%struct.VectorInt* %lhs, %struct.VectorInt* %rhs) {"""
    vectorCode += """  %1 = alloca %struct.VectorInt*, align 8"""
    vectorCode += """  %2 = alloca %struct.VectorInt*, align 8"""
    vectorCode += """  store %struct.VectorInt* %lhs, %struct.VectorInt** %1, align 8"""
    vectorCode += """  store %struct.VectorInt* %rhs, %struct.VectorInt** %2, align 8"""
    vectorCode += """  %3 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  %4 = getelementptr inbounds %struct.VectorInt* %3, i32 0, i32 2"""
    vectorCode += """  %5 = load i32** %4, align 8"""
    vectorCode += """  %6 = load %struct.VectorInt** %2, align 8"""
    vectorCode += """  %7 = getelementptr inbounds %struct.VectorInt* %6, i32 0, i32 2"""
    vectorCode += """  %8 = load i32** %7, align 8"""
    vectorCode += """  %9 = icmp ne i32* %5, %8"""
    vectorCode += """  br i1 %9, label %10, label %12"""
    vectorCode += """  """
    vectorCode += """  ; <label>:10                                      ; preds = %0"""
    vectorCode += """  %11 = load %struct.VectorInt** %1, align 8"""
    vectorCode += """  call void @_Z11vector_freeP9VectorInt(%struct.VectorInt* %11)"""
    vectorCode += """  br label %12"""
    vectorCode += """  """
    vectorCode += """  ; <label>:12                                      ; preds = %10, %0"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
    vectorCode += """  ; Function Attrs: ssp uwtable"""
    vectorCode += """define void @_Z11vector_initP13VectorBoolean(%struct.VectorBoolean* %vector) #0 {"""
    vectorCode += """  %1 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 0"""
    vectorCode += """  store i32 0, i32* %3, align 4"""
    vectorCode += """  %4 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %5 = getelementptr inbounds %struct.VectorBoolean* %4, i32 0, i32 1"""
    vectorCode += """  store i32 100, i32* %5, align 4"""
    vectorCode += """  %6 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %7 = getelementptr inbounds %struct.VectorBoolean* %6, i32 0, i32 3"""
    vectorCode += """  store i8 0, i8* %7, align 1"""
    vectorCode += """  %8 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %9 = getelementptr inbounds %struct.VectorBoolean* %8, i32 0, i32 1"""
    vectorCode += """  %10 = load i32* %9, align 4"""
    vectorCode += """  %11 = sext i32 %10 to i64"""
    vectorCode += """  %12 = mul i64 1, %11"""
    vectorCode += """  %13 = call i8* @malloc(i64 %12)"""
    vectorCode += """  %14 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %15 = getelementptr inbounds %struct.VectorBoolean* %14, i32 0, i32 2"""
    vectorCode += """  store i8* %13, i8** %15, align 8"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
    vectorCode += """  ; Function Attrs: ssp uwtable"""
    vectorCode += """define void @_Z11vector_freeP13VectorBoolean(%struct.VectorBoolean* %vector) #0 {"""
    vectorCode += """  %1 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 2"""
    vectorCode += """  %4 = load i8** %3, align 8"""
    vectorCode += """  %5 = icmp ne i8* %4, null"""
    vectorCode += """  br i1 %5, label %6, label %15"""
    vectorCode += """  """
    vectorCode += """  ; <label>:6                                       ; preds = %0"""
    vectorCode += """  %7 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %8 = getelementptr inbounds %struct.VectorBoolean* %7, i32 0, i32 3"""
    vectorCode += """  %9 = load i8* %8, align 1"""
    vectorCode += """  %10 = trunc i8 %9 to i1"""
    vectorCode += """  br i1 %10, label %15, label %11"""
    vectorCode += """  """
    vectorCode += """  ; <label>:11                                      ; preds = %6"""
    vectorCode += """  %12 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %13 = getelementptr inbounds %struct.VectorBoolean* %12, i32 0, i32 2"""
    vectorCode += """  %14 = load i8** %13, align 8"""
    vectorCode += """  call void @free(i8* %14)"""
    vectorCode += """  br label %15"""
    vectorCode += """  """
    vectorCode += """  ; <label>:15                                      ; preds = %11, %6, %0"""
    vectorCode += """  %16 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %17 = getelementptr inbounds %struct.VectorBoolean* %16, i32 0, i32 2"""
    vectorCode += """  store i8* null, i8** %17, align 8"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
    vectorCode += """  ; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define i32 @_Z3lenP13VectorBoolean(%struct.VectorBoolean* %vector) #3 {"""
    vectorCode += """  %1 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 0"""
    vectorCode += """  %4 = load i32* %3, align 4"""
    vectorCode += """  ret i32 %4"""
    vectorCode += """}"""
    vectorCode += """  ; Function Attrs: nounwind ssp uwtable"""
    vectorCode += """define void @_Z19vector_set_borrowedP13VectorBoolean(%struct.VectorBoolean* %vector) #3 {"""
    vectorCode += """  %1 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  store %struct.VectorBoolean* %vector, %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %2 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %3 = getelementptr inbounds %struct.VectorBoolean* %2, i32 0, i32 3"""
    vectorCode += """  store i8 1, i8* %3, align 1"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
    vectorCode += """; Function Attrs: ssp uwtable"""
    vectorCode += """define void @_Z23vector_free_lhs_if_diffP13VectorBooleanP13VectorBoolean(%struct.VectorBoolean* %lhs, %struct.VectorBoolean* %rhs) {"""
    vectorCode += """  %1 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  %2 = alloca %struct.VectorBoolean*, align 8"""
    vectorCode += """  store %struct.VectorBoolean* %lhs, %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  store %struct.VectorBoolean* %rhs, %struct.VectorBoolean** %2, align 8"""
    vectorCode += """  %3 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  %4 = getelementptr inbounds %struct.VectorBoolean* %3, i32 0, i32 2"""
    vectorCode += """  %5 = load i8** %4, align 8"""
    vectorCode += """  %6 = load %struct.VectorBoolean** %2, align 8"""
    vectorCode += """  %7 = getelementptr inbounds %struct.VectorBoolean* %6, i32 0, i32 2"""
    vectorCode += """  %8 = load i8** %7, align 8"""
    vectorCode += """  %9 = icmp ne i8* %5, %8"""
    vectorCode += """  br i1 %9, label %10, label %12"""
    vectorCode += """  """
    vectorCode += """  ; <label>:10                                      ; preds = %0"""
    vectorCode += """  %11 = load %struct.VectorBoolean** %1, align 8"""
    vectorCode += """  call void @_Z11vector_freeP13VectorBoolean(%struct.VectorBoolean* %11)"""
    vectorCode += """  br label %12"""
    vectorCode += """  """
    vectorCode += """  ; <label>:12                                      ; preds = %10, %0"""
    vectorCode += """  ret void"""
    vectorCode += """}"""
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
      debugCode.toSeq                 ++
      copyCode.toSeq                  ++
      cinterface.toSeq

    (genRes, e.id, Map())
  }

  private def gen(n: ast.Nop, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) =
    (Seq(), e.id - 1, Map())

  private def gen(n: ast.App, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val NodeMeta(typeId, Some(sym@Symbol(sc, nm, ts))) = m.get(n)
    val tp = typeId.toLLVMType
    val (ps, dd, id1, heap1) = n.args.foldLeft(
      Seq[String](),
      Seq[Int](),
      e.id - 1,
      HeapStore()) {
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
        (ps ++ callGen ++ allocGen, id3, heap1 ++ Map((typeId, id3.toString) -> id3.toString))
      case _ =>
        (ps ++ callGen, id2, heap1)
    }
  }

  private def genAllocStore(id: Int, v: String, tp: Type)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val alloc = s"%$id = alloca ${tp.toLLVMType}"
    val store = s"store ${tp.toLLVMType} $v, ${tp.toLLVMType}* %$id"
    (Seq(alloc, store), id, Map())
  }

  private def gen(n: ast.Cond, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
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
      HeapStore()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }

    val (bodyStore, finalId) =
      if (tp.startsWith("%struct.")) {
        val call = s"%${id2 + 1} = load $tp* %${id2}"
        val store = s"store $tp %${id2 + 1}, $tp* %${e.id}"
        (Seq(call, store).mkString("  \n  "), id2 + 1)
      }
      else if (tp != "void")
        (s"store $tp %${id2}, $tp* %${e.id}", id2)
      else
        ("", id2)

    val elseId = finalId + 1

    val condBr =
      if (tp.startsWith("%struct.")) {
        s"br i1 %$id1, label %$bodyId, label %${elseId}"
      }
      else
        s"br i1 %$id1, label %$bodyId, label %$elseId"

    val (g, id3, heap3) = n.others.foldLeft(
      Seq[Seq[String]](),
      elseId,
      HeapStore()) {
      case ((ss, id, hh), n) =>
        // Needs to skip 2 instruction ids in order to provide space
        // for label ids.
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        val (withStore, newFinalId) =
          if (tp.startsWith("%struct.VectorBoolean")) {
            val call = s"call void @__copyB($tp* %$newId, $tp* %${e.id})"
            if (s.last.startsWith("call void @_Z19vector_set_borrowed")) {
              (s ++ Seq(call) :+ s"call void @_Z19vector_set_borrowedP13VectorBoolean(%struct.VectorBoolean* %${e.id})", newId)
            }
            else {
              (s ++ Seq(call), newId)
            }

          }

          else if (tp.startsWith("%struct.VectorInt")) {
            val call = s"call void @__copyI($tp* %$newId, $tp* %${e.id})"
            if (s.last.startsWith("call void @_Z19vector_set_borrowed")) {
              (s ++ Seq(call) :+ s"call void @_Z19vector_set_borrowedP9VectorInt(%struct.VectorInt* %${e.id})", newId)
            }
            else {
              (s ++ Seq(call), newId)
            }
            (s ++ Seq(call), newId)
          }
          else if (tp != "void")
            (s :+ s"store $tp %${newId}, $tp* %${e.id}", newId)
          else
            (s, newId)

        (ss :+ withStore, newFinalId + 1, hh ++ h)
    }


    val newIns = g.map(_ :+ s"br label %$id3")
    val (newRes, id4) =
      if (tp.startsWith("%struct.")) {
        val load = s"%${id3 + 1} = load $tp* %${e.id}"
        val (g, id, _) = genAllocStore(id3 + 2, s"%${id3 + 1}", retTp)
        (load +: g, id)
      }
      else if (tp != "void") {
        (Seq(s"%${id3 + 1} = load $tp* %${e.id}"), id3 + 1)
      }
      else
       (Seq(), id3)

    val allocNamesInScope = n.body.collect {
      case n: ast.Let => s"${n.name}${n.pos.row}${n.pos.col}"
      case n: ast.Mut => s"${n.name}${n.pos.row}${n.pos.col}"
    }

    val frees = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, finalId.toString)
        val allHeap = heap1 ++ heap2

        val heapId = allHeap.get(retHeap) getOrElse ""
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, _), hid) => hid != heapId }
          .filter { case ((tp, nn), hid) =>
              (allocNamesInScope contains hid) || nn == hid }
          .map { case ((tp, _), hid) => (tp, hid) }
        heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
      case primitives =>
        val allHeap = heap1 ++ heap2
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, nn), hid) =>
              (allocNamesInScope contains hid) || nn == hid }
          .map { case ((tp, _), hid) => (tp, hid) }
        heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
    }

    val stillNeedHeap: HeapStore = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, finalId.toString)
        val allHeap = heap1 ++ heap2 ++ heap3
        val heapId = allHeap.get(retHeap) getOrElse ""
        if (allocNamesInScope contains heapId)
          Map((retTp, id4.toString) -> heapId)
        else
          HeapStore()
      case others =>
        HeapStore()
    }

    val changeToBorrowed: String = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, finalId.toString)
        val allHeap = heap1 ++ heap2 ++ heap3
        val heapId = allHeap.get(retHeap) getOrElse ""
        if (! (allocNamesInScope contains heapId)) {
          val fn = genCFnName("vector_set_borrowed", Seq(retTp), types.Var("Unit"))
          s"call $fn($tp* %${e.id})"
        }
        else
          ""
      case others =>
        ""
    }

    (Seq(alloc) ++
     (condGen :+ condBr) ++
     (bodyGen :+ bodyStore) ++
     (frees.toSet) ++
     Seq(changeToBorrowed) ++
     (Seq() :+ s"br label %$id3") ++
     (newIns.flatten) ++
     (newRes), id4, stillNeedHeap)
   }

  private def gen(n: ast.Elif, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val (condGen, id1, heap1) = gen(n.cond, e)
    val bodyId = id1 + 1
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      bodyId,
      HeapStore()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }
    var elseId = id2 + 1
    var condBr = s"br i1 %$id1, label %$bodyId, label %$elseId"

    val allocNamesInScope = n.body.collect {
      case n: ast.Let => s"${n.name}${n.pos.row}${n.pos.col}"
      case n: ast.Mut => s"${n.name}${n.pos.row}${n.pos.col}"
    }

    val frees = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, id2.toString)
        val allHeap = heap1 ++ heap2

        val heapId = allHeap.get(retHeap) getOrElse ""
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, _), hid) => hid != heapId }
          .filter { case ((tp, nn), hid) =>
              (allocNamesInScope contains hid) || nn == hid }
          .map { case ((tp, _), hid) => (tp, hid) }
        heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
      case primitives =>
        val allHeap = heap1 ++ heap2
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, nn), hid) =>
              (allocNamesInScope contains hid) || nn == hid }
          .map { case ((tp, _), hid) => (tp, hid) }
        heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
    }

    val stillNeedHeap: HeapStore = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, id2.toString)
        val allHeap = heap1 ++ heap2
        val heapId = allHeap.get(retHeap) getOrElse ""
        if (allocNamesInScope contains heapId)
          Map(retHeap -> heapId)
        else
          HeapStore()
      case others =>
        HeapStore()
    }

    val changeToBorrowed: String = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, id2.toString)
        val allHeap = heap1 ++ heap2
        val heapId = allHeap.get(retHeap) getOrElse ""
        if (! (allocNamesInScope contains heapId)) {
          val fn = genCFnName("vector_set_borrowed", Seq(retTp), types.Var("Unit"))
          s"call $fn($tp* %1)"
        }
        else
          ""
      case others =>
        ""
    }

    ((condGen :+ condBr) ++ bodyGen ++ frees.toSet ++ Seq(changeToBorrowed), id2, stillNeedHeap)
   }

  private def gen(n: ast.Else, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val (bodyGen, id2, heap2) = n.body.foldLeft(
      Seq[String](),
      e.id - 1,
      HeapStore()) {
      case ((ss, id, hh), n) =>
        val (s, newId, h) = gen(n, e.copy(id = id + 1))
        (ss ++ s, newId, hh ++ h)
    }

    val allocNamesInScope = n.body.collect {
      case n: ast.Let => s"${n.name}${n.pos.row}${n.pos.col}"
      case n: ast.Mut => s"${n.name}${n.pos.row}${n.pos.col}"
    }

    val frees = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, id2.toString)
        val allHeap = heap2

        val heapId = allHeap.get(retHeap) getOrElse ""
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, _), hid) => hid != heapId }
          .filter { case ((tp, nn), hid) =>
              (allocNamesInScope contains hid) || nn == hid }
          .map { case ((tp, _), hid) => (tp, hid) }
        heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
      case primitives =>
        val allHeap = heap2
        val heapSeq = allHeap.toSeq
          .filter { case ((tp, nn), hid) =>
              (allocNamesInScope contains hid) || nn == hid }
          .map { case ((tp, _), hid) => (tp, hid) }
        heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
    }

    val stillNeedHeap: HeapStore = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, id2.toString)
        val allHeap = heap2
        val heapId = allHeap.get(retHeap) getOrElse ""
        if (allocNamesInScope contains heapId)
          Map(retHeap -> heapId)
        else
          HeapStore()
      case others =>
        HeapStore()
    }

    val changeToBorrowed: String = tp match {
      case struct if struct.startsWith("%struct.") =>
        val retHeap = (retTp, id2.toString)
        val allHeap = heap2
        val heapId = allHeap.get(retHeap) getOrElse ""
        if (! (allocNamesInScope contains heapId)) {
          val fn = genCFnName("vector_set_borrowed", Seq(retTp), types.Var("Unit"))
          s"call $fn($tp* %1)"
        }
        else
          ""
      case others =>
        ""
    }

    (bodyGen ++ frees.toSet ++ Seq(changeToBorrowed), id2, stillNeedHeap)
   }

  private def gen(n: ast.While, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val loopId = e.id
    val loopBr = s"br label %$loopId"
    val (condGen, id1, heap1) = gen(n.cond, e.copy(id = loopId + 1))
    val bodyId = id1 + 1
    val (bodyGen, ids, heap2) = n.body.generate(e.copy(id = bodyId + 1))
    val id2 = ids.last
    val lastId = id2 + 1
    val condBr = s"br i1 %$id1, label %$bodyId, label %$lastId"

    val allocNamesInScope = n.body.collect {
      case n: ast.Let => s"${n.name}${n.pos.row}${n.pos.col}"
      case n: ast.Mut => s"${n.name}${n.pos.row}${n.pos.col}"
    }

    val frees = {
      val allHeap = heap1 ++ heap2
      val heapSeq = allHeap.toSeq
        .filter { case ((tp, nn), hid) =>
          (allocNamesInScope contains hid) || nn == hid }
        .map { case ((tp, _), hid) => (tp, hid) }
      heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
    }

    ( Seq(loopBr)         ++
      (condGen :+ condBr) ++
      bodyGen             ++
      frees.toSet         ++
      Seq(loopBr), lastId, Map())
  }

  private def gen(n: ast.For, e: Env)
   (implicit m: NodeMap): (Seq[String], Int, HeapStore) = {
    val retTp = m.get(n).typeid
    val tp = retTp.toLLVMType
    val it = s"${n.name}${n.pos.row}${n.pos.col}"
    val iter = s"%$it = alloca i32"
    val (fromGen, id1, heap1) = gen(n.from, e)
    val storeIt = s"store i32 %$id1, i32* %$it"
    val loopId = id1 + 1
    val loopBr = s"br label %$loopId"
    val (toGen  , id2, heap2) = gen(n.to, e.copy(id = loopId + 1))
    val id3 = id2 + 1
    val id4 = id3 + 1
    val id5 = id4 + 1

    val loadIt = s"%$id3 = load i32* %$it"
    val loadTo = s"%$id4 = add nsw i32 %$id2, 0"
    val cmp = s"%$id5 = icmp slt i32 %$id3, %$id4"
    val cmpGen = Seq(loadIt, loadTo, cmp)

    val bodyId = id5 + 1
    val (bodyGen, ids, heap3) = n.body.generate(e.copy(id = bodyId + 1))
    val id6 = ids.last
    val id7 = id6 + 1
    val bodyBr = s"br label %$id7"
    val id8 = id7 + 1

    val load = s"%$id7 = load i32* %$it"
    val addi = s"%$id8 = add nsw i32 %$id7, 1"
    val stor = s"store i32 %$id8, i32* %$it"
    val incr = Seq(load, addi, stor)

    val lastId = id8 + 1
    val condBr = s"br i1 %$id5, label %$bodyId, label %$lastId"

    val allocNamesInScope = n.body.collect {
      case n: ast.Let => s"${n.name}${n.pos.row}${n.pos.col}"
      case n: ast.Mut => s"${n.name}${n.pos.row}${n.pos.col}"
    }

    val frees = {
      val allHeap = heap1 ++ heap2 ++ heap3
      val heapSeq = allHeap.toSeq
        .filter { case ((tp, nn), hid) =>
          (allocNamesInScope contains hid) || nn == hid }
        .map { case ((tp, _), hid) => (tp, hid) }
      heapSeq.map { case (th, h) => genFreeMemStruct(e, h, th) }
    }

    ( Seq(iter) ++
      fromGen      ++
      Seq(storeIt) ++
      Seq(loopBr)  ++
      toGen        ++
      cmpGen       ++
      Seq(condBr)  ++
      bodyGen      ++
      frees.toSet  ++
      incr         ++
      Seq(loopBr), lastId, Map())
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
