package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object ExecutionContextLattice extends PowerSubSetLattice[(List[ObjectLabel], ObjectLabel)] {
  
  /* Constructors */  
  def makeElement(scopeChain: List[ObjectLabel], variableObject: ObjectLabel): ExecutionContextLattice.Elt =
    makeElt(Set((scopeChain, variableObject)))
  
  /* Getters */
  def getVariableObject(el: ExecutionContextLattice.Elt): Set[ObjectLabel] = el match {
    case Concrete(e) => e.map({ case (_, variableObject) => variableObject })
    case _ => throw new NotImplementedException()
  }
    
  /* Setters */
  def setVariableObject(el: ExecutionContextLattice.Elt, label: ObjectLabel): ExecutionContextLattice.Elt =
    throw new NotImplementedException()
}
