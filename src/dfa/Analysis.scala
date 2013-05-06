package tapy.dfa

import tapy.cfg._
import tapy.dfa.MonotoneFrameworkTypes._

trait Analysis[T] {
  def generateConstraint(cfgNode: Node): Constraint[T]
  def nodeDependencies(cfgNode: Node): Set[Node]
}