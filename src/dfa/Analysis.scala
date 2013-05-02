package tapy.dfa

import tapy.cfg._
import tapy.mfw.MonotoneFrameworkTypes._

trait Analysis[T] {
  def generateConstraint(cfgNode: Node): Constraint[T]
  def nodeDependencies(cfgNode: Node): List[Node]
}
