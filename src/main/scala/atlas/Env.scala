package atlas

import atlas.types.Type
import atlas.ast.Node

case class Env(archive: Map[(Node, LinePos), Type], context: Context)

object Env {
  def apply(): Env = Env(Map(), Context())
}