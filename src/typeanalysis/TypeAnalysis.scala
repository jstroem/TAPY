package tapy.typeanalysis

import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[AnalysisLattice.Elt] {
  type Elt = AnalysisLattice.Elt
  
  def bottom = AnalysisLattice.bottom
  
  def generateConstraint(node: Node): Constraint[Elt] = {
    return node match {
        case node: ConstantStringNode => ((solution) =>
          handleConstantString(node, solution.getOrElse(node, bottom)))
        
        case node => ((solution) =>
          solution.getOrElse(node, AnalysisLattice.bottom))
      }
  }
  
  def handleConstantString(node: ConstantStringNode, currentSolution: Elt): Elt = {
    val (programState, callGraph) = currentSolution
    val state = ProgramStateLattice.get(programState, node)
    
    
    
    return currentSolution
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getPredecessors(cfgNode)
  }
}