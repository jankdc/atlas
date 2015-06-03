package object atlas {

  import atlas.ast.Node
  import atlas.types.Type
  import atlas.errors.TypingError

  case class LinePos(row: Int, col: Int) {
    override def toString = f"[$row%2d, $col%2d]"
  }

  case class Context(
    defTypes: Set[String]       = Set(),
    bindings: Map[Symbol, Type] = Map()) {

    def addDef(s: Symbol, t: Type): Context = {
      checkType(s.pos, t)

      bindings.get(s) match {
        case None =>
          this.copy(bindings = this.bindings + (s -> t))
        case some =>
          throw TypingError(s"${s.pos}: ${s.name} is already defined.")
      }
    }

    def getDef(s: String, p: LinePos): (Symbol, Type) =
      bindings
        .toSeq
        .sortBy { case (sym, _) => -sym.pos.col }
        .find { case (Symbol(_, nm, op), _) => nm + op == s }
        .getOrElse { throw TypingError(s"$p: $s is undefined.") }

    def mkType(s: String): Context =
      this.copy(defTypes = this.defTypes + s)

    private def checkType(p: LinePos, t: Type): Unit = t match {
      case types.Fun(terms) => terms.foreach(checkType(p, _))
      case types.Var(tname) =>
        if (! defTypes.contains(tname))
          throw TypingError(s"$p: Type is not found: $tname")
      case types.List(tp) => checkType(p, tp)
    }
  }

  case class NodeMeta(typeid: Type, sym: Option[Symbol])

  case class NodeMap(map: Map[(Node, LinePos), NodeMeta]) {
    def get(n: Node): NodeMeta = map((n, n.pos))
    def add(n: Node, m: NodeMeta): NodeMap = this.copy(map + ((n, n.pos) -> m))
  }

  case class Symbol(scope: String, name: String, opt: String = "")(
    val pos: LinePos,
    val isStatic: Boolean,
    val isConstant: Boolean,
    val scopeLevel: Int,
    val returnsParam: Boolean = false
  )

}
