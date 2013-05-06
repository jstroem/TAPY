package tapy.typeanalysis

import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._

object TypeAnalysis {
  val lattice = new AnalysisLattice[Int, String, Node]()
  val product = new ProductLattice[tapy.lattices.NoneLattice, NoneLattice]()
}

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[TypeAnalysis.lattice.Elt] {
  def generateConstraint(node: Node): Constraint[UndefinedElt] = {
    return node match {
        case node: ConstantStringNode => ((solution: Solution[UndefinedElt]) => handleConstantString(node))
        case node => ((solution) => solution.getOrElse(node, new UndefinedLattice().bottom))
      }
  }
  
  def handleConstantString(node: ConstantStringNode): UndefinedElt = {
    return null
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getPredecessors(cfgNode)
  }
}