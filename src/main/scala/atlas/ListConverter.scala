package atlas


object ListFilter {
  def filterFun(ns: Iterable[nodes.Node]): Iterable[nodes.Fun] = ns flatMap {
    case n: nodes.Fun => Some(n)
    case other        => None
  }

  def filterLet(ns: Iterable[nodes.Node]): Iterable[nodes.Let] = ns flatMap {
    case n: nodes.Let => Some(n)
    case other        => None
  }

  def filterAbs(ns: Iterable[types.Type]): Iterable[types.Abs] = ns flatMap {
    case n: types.Abs => Some(n)
    case other        => None
  }

  def filterParam(ns: Iterable[nodes.Node]): Iterable[nodes.Param]  = ns flatMap {
    case n: nodes.Param => Some(n)
    case other          => None
  }
}
