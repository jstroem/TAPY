package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object ExecutionContextLattice extends PowerSubSetLattice[(List[ObjectLabel], ObjectLabel)] {
  
  /* Getters */
  
  def getVariableObject(el: ExecutionContextLattice.Elt): Set[ObjectLabel] =
    el.map({ case (_, variableObject) => variableObject })
  
  def getVariableObjectsOnScopeChains(el: ExecutionContextLattice.Elt): Set[List[ObjectLabel]] =
    el.map({ case (scopeChain, variableObject) => variableObject :: scopeChain })
    
  /* Setters */
  
  def setVariableObject(el: ExecutionContextLattice.Elt, label: ObjectLabel): ExecutionContextLattice.Elt =
    throw new NotImplementedException()
}
