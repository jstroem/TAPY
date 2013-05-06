package tapy.typeanalysis

import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._

object TypeAnalysis {
  val lattice = new AnalysisLattice[Int, String, Node]()
}

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[TypeAnalysis.lattice.Elt] {
  type Elt = TypeAnalysis.lattice.Elt
  
  def bottom = TypeAnalysis.lattice.bottom
  
  def generateConstraint(node: Node): Constraint[Elt] = {
    return node match {
        case node: ConstantStringNode => ((solution) => handleConstantString(node, solution.getOrElse(node, bottom)))
        case node => ((solution) => solution.getOrElse(node, TypeAnalysis.lattice.bottom))
      }
  }
  
  def handleConstantString(node: ConstantStringNode, currentSolution: Elt): Elt = {
    return currentSolution
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getPredecessors(cfgNode)
  }
}