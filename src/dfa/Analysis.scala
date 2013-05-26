package tapy.dfa

import tapy.cfg._
import tapy.dfa.MonotoneFrameworkTypes._

trait Analysis[T] {
  var cfg: ControlFlowGraph = null
  var worklist: Worklist[T] = null
  
  def generateConstraint(node: Node): Constraint[T]
  def nodeDependencies(node: Node, solution: T): Set[Node]
}