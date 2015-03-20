package atlas

import atlas.ast.Node
import atlas.types.Type
import scala.collection.mutable

object Context {
  def apply(): Context = Context(Set(), Map())
}

case class Context(typeset: Set[String], bindings: Map[Sym, Seq[Type]]) {

  def addVar(s: Sym, t: Type): Context = {
    if (! isTypeBound(t))
      throw CheckError(s"${s.pos}: Type is not found: $t")

    bindings.get(s) match {
      case None =>
        this.copy(bindings = this.bindings + (s -> Seq(t)))
      case some =>
        throw CheckError(s"${s.pos}: ${s.name} is already defined.")
    }
  }

  def getVar(s: Sym): Type =
    bindings
      .toSeq
      .collect { case (Sym(_, sm, _), ts) if sm == s.name => ts.collectFirst {
        case t: types.Var => t
        case t: types.Fun if t.terms.length == 1 => t }}
      .flatten
      .headOption
      .getOrElse(throw CheckError(s"${s.pos}: ${s.name} is undefined."))

  def addFun(s: Sym, t: Type): Context = {
    if (! isTypeBound(t))
      throw CheckError(s"${s.pos}: Type is not found: $t")

    bindings.get(s) match {
      case None =>
        this.copy(bindings = this.bindings + (s -> Seq(t)))
      case Some(ts) if ! ts.contains(t) =>
        this.copy(bindings = this.bindings + (s -> (t +: ts)))
      case Some(ts) =>
        throw CheckError(s"${s.pos}: ${s.name} is already defined.")
    }
  }

  def getFun(s: Sym, as: Seq[Type]): (String, Type) =
    bindings
      .toSeq
      .collect { case (Sym(sc, sm, _), ts) if sm == s.name => ts.collectFirst {
        case t@types.Fun(ps :+ rt) if ps == as => (sc, rt) }}
      .flatten
      .headOption
      .getOrElse(throw CheckError(s"${s.pos}: ${s.name} is undefined."))

  def addType(s: String): Context =
    this.copy(typeset = this.typeset + s)

  private def isTypeBound(t: Type): Boolean = t match {
    case types.Fun(terms) => terms forall isTypeBound
    case types.Var(tname) => typeset contains tname
    case types.App(_, ts) => isTypeBound(ts)
  }
}

// class Context
// (
//   private val simTypes: mutable.Set[Type] = mutable.Set(),
//   private val varBinds: mutable.Map[String, Type] = mutable.Map(),
//   private val absBinds: mutable.Map[String, Seq[types.Abs]] = mutable.Map()
// )
// {
//   // PRELUDE TYPES
//   simTypes += types.Var("Int")
//   simTypes += types.Var("Unit")

//   def addVar(tag: Tag, tp: Type): Unit =
//     if (! isVarBound(tag.name))
//       varBinds += (tag.name -> tp)
//     else
//       throw CheckError(s"${tag.pos}: ${tag.name} is already defined.")

//   def getVar(tag: Tag): Type = {
//     (varBinds.get(tag.name)) match {
//       case Some(typeid) =>
//         typeid
//       case None =>
//         throw CheckError(s"${tag.pos}: Var is undefined: ${tag.name}")
//     }
//   }

//   def addAbs(tag: Tag, tp: types.Abs): Unit = {
//     if (! isAbsLegal(tag.name, tp)) {
//       throw CheckError(s"${tag.pos}: ${tag.name} has illegal signature.")
//     }

//     if (! isAbsBound(tag.name, tp))
//       absBinds += (tag.name -> (tp +: abs(tag.name)))
//     else
//       throw CheckError(s"${tag.pos}: ${tag.name} is already defined.")
//   }

//   def getApp(tag: Tag, args: Seq[Type]): Type = {
//     if (! isAbsBound(tag.name))
//       throw CheckError(s"${tag.pos}: ${tag.name} is undefined.")

//     if (isAbsLegal(tag.name, args))
//       abs(tag.name).filter(_.terms.init == args).head.terms.last
//     else {
//       throw CheckError(s"${tag.pos}: ${tag.name} has wrong arguments.")
//     }
//   }

//   def mkType(n: Node): Type =
//     n match {
//       case ast.NamedId(nm) if isTypeBound(types.Var(nm)) =>
//         types.Var(nm)
//       case ast.NamedId(nm) =>
//         throw CheckError(s"[${n.pos}]: Type not found: $nm")
//       case ast.Type(Seq(node)) =>
//         mkType(node)
//       case ast.Type(nodes) =>
//         types.Abs(nodes map mkType)
//       case others =>
//         assert(false, s"ERROR: NODE MUST BE TYPEABLE: $n")
//         ???
//     }


//   override def clone(): Context = new Context(
//     mutable.Set(simTypes.toSeq: _*),
//     mutable.Map(varBinds.toSeq: _*),
//     mutable.Map(absBinds.toSeq: _*)
//   )

//   private type Tag = Node with Bound

//   private def abs(s: String) = {
//     val defAbs = absBinds.get(s) getOrElse Seq()
//     val varAbs = varBinds.get(s) match {
//       case Some(tp: types.Abs) => Seq(tp)
//       case other => Seq()
//     }
//     defAbs ++ varAbs
//   }

//   private def isTypeBound(tp: Type): Boolean = tp match {
//     case types.Fun(_, ts) => simTypes contains tp
//     case types.Var(_) => simTypes.contains(tp)
//     case types.Abs(m) => m.forall(simTypes.contains(_))
//   }

//   private def isAbsBound(s: String) = ! abs(s).isEmpty
//   private def isAbsBound(s: String, tp: types.Abs) = ! abs(s).filter(_ == tp).isEmpty
//   private def isAbsLegal(s: String, tp: types.Abs) = abs(s).forall(_.terms.last == tp.terms.last)
//   private def isAbsLegal(s: String, ts: Seq[Type]) = ! abs(s).filter(_.terms.init == ts).isEmpty
//   private def isVarBound(s: String) = ! varBinds.get(s).isEmpty
// }
