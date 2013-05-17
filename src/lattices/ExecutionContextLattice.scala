package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object ExecutionContextLattice extends PowerSubSetLattice[(List[ObjectLabel], ObjectLabel)] {
  
  /* Setters */
  
  def popVariableObject(el: Elt): Elt =
    if (el != null)
      el.map({
        case (newVariableObject :: scopeChain, oldVariableObject) => (scopeChain, newVariableObject)
        case _ => throw new InternalError()
      })
    else Set()
  
  /* Getters */
  
  def getVariableObjects(el: Elt): Set[ObjectLabel] =
    if (el != null) el.map({ case (_, variableObject) => variableObject }) else Set()
  
  def getVariableObjectsOnScopeChains(el: Elt): Set[List[ObjectLabel]] =
    if (el != null) el.map({ case (scopeChain, variableObject) => variableObject :: scopeChain }) else Set()
}
