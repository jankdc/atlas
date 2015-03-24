package atlas

import atlas.ast.Node

case class NodeMap(map: Map[(Node, LinePos), NodeMeta]) {
  def get(n: Node): NodeMeta = map((n, n.pos))
  def add(n: Node, m: NodeMeta): NodeMap = this.copy(map + ((n, n.pos) -> m))
}
