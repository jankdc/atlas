package atlas

import atlas.ast.Node
import atlas.types.Type
import collection.mutable

class Context
(
  private val simTypes: mutable.Set[Type] = mutable.Set(),
  private val varBinds: mutable.Map[String, Type] = mutable.Map(),
  private val absBinds: mutable.Map[String, Seq[types.Abs]] = mutable.Map()
)
{
  // PRELUDE TYPES
  simTypes += types.Var("Int")
  simTypes += types.Var("Unit")

  def addVar(tag: Tag, tp: Type): Unit =
    if (! isVarBound(tag.name))
      varBinds += (tag.name -> tp)
    else
      throw CheckerError(s"${tag.pos}: ${tag.name} is already defined.")

  def getVar(tag: Tag): Type = {
    (varBinds.get(tag.name)) match {
      case Some(typeid) =>
        typeid
      case None =>
        throw CheckerError(s"${tag.pos}: Var is undefined: ${tag.name}")
    }
  }

  def addAbs(tag: Tag, tp: types.Abs): Unit = {
    if (! isAbsLegal(tag.name, tp)) {
      throw CheckerError(s"${tag.pos}: ${tag.name} has illegal signature.")
    }

    if (! isAbsBound(tag.name, tp))
      absBinds += (tag.name -> (tp +: abs(tag.name)))
    else
      throw CheckerError(s"${tag.pos}: ${tag.name} is already defined.")
  }

  def getApp(tag: Tag, args: Seq[Type]): Type = {
    if (! isAbsBound(tag.name))
      throw CheckerError(s"${tag.pos}: ${tag.name} is undefined.")

    if (isAbsLegal(tag.name, args))
      abs(tag.name).filter(_.terms.init == args).head.terms.last
    else {
      throw CheckerError(s"${tag.pos}: ${tag.name} has wrong arguments.")
    }
  }

  def mkType(n: Node): Type =
    n match {
      case ast.NamedId(nm) if isTypeBound(types.Var(nm)) =>
        types.Var(nm)
      case ast.NamedId(nm) =>
        throw CheckerError(s"[${n.pos}]: Type not found: $nm")
      case ast.Sig(Seq(node)) =>
        mkType(node)
      case ast.Sig(nodes) =>
        types.Abs(nodes map mkType)
      case others =>
        assert(false, s"ERROR: NODE MUST BE TYPEABLE: $n")
        ???
    }


  override def clone(): Context = new Context(
    mutable.Set(simTypes.toSeq: _*),
    mutable.Map(varBinds.toSeq: _*),
    mutable.Map(absBinds.toSeq: _*)
  )

  private type Tag = Node with Bound

  private def abs(s: String) = {
    val defAbs = absBinds.get(s) getOrElse Seq()
    val varAbs = varBinds.get(s) match {
      case Some(tp: types.Abs) => Seq(tp)
      case other => Seq()
    }
    defAbs ++ varAbs
  }

  private def isTypeBound(tp: Type): Boolean = tp match {
    case types.Var(_) => simTypes.contains(tp)
    case types.Abs(m) => m.forall(simTypes.contains(_))
  }

  private def isAbsBound(s: String) = ! abs(s).isEmpty
  private def isAbsBound(s: String, tp: types.Abs) = ! abs(s).filter(_ == tp).isEmpty
  private def isAbsLegal(s: String, tp: types.Abs) = abs(s).forall(_.terms.last == tp.terms.last)
  private def isAbsLegal(s: String, ts: Seq[Type]) = ! abs(s).filter(_.terms.init == ts).isEmpty
  private def isVarBound(s: String) = ! varBinds.get(s).isEmpty
}
