package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object ProgramStateLattice extends MapLattice[Node, StateLattice.Elt](StateLattice) {
  
  /* Getters */
  
  def getState(el: ProgramStateLattice.Elt, node: Node): StateLattice.Elt =
    get(el, node)
  
  def getVariableObjects(el: ProgramStateLattice.Elt, node: Node): Set[ObjectLabel] =
    StateLattice.getVariableObjects(getState(el, node))
    
  /* Setters */
  
  def setExecutionContext(el: ProgramStateLattice.Elt, node: Node, executionContext: ExecutionContextLattice.Elt): ProgramStateLattice.Elt =
    update(el, node, StateLattice.setExecutionContext(getState(el, node), executionContext))
}