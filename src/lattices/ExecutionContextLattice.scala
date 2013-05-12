package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object ExecutionContextLattice extends PowerSubSetLattice[(List[ObjectLabel], ObjectLabel)] {
  
  /* Getters */
  
  def getVariableObjects(el: ExecutionContextLattice.Elt): Set[ObjectLabel] =
    if (el != null) el.map({ case (_, variableObject) => variableObject }) else Set()
  
  def getVariableObjectsOnScopeChains(el: ExecutionContextLattice.Elt): Set[List[ObjectLabel]] =
    if (el != null) el.map({ case (scopeChain, variableObject) => variableObject :: scopeChain }) else Set()
}
