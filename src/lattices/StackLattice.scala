package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object StackLattice extends ProductLattice(StackFrameLattice, ExecutionContextLattice) {

  /* Getters */
  def getStackFrame(el: Elt) : StackFrameLattice.Elt = {
    val (stackframe,_) = el
    return stackframe
  }

  def getExecutionContext(el: Elt) : ExecutionContextLattice.Elt = {
    val (_,executionContext) = el
    executionContext
  }

  def getVariableObjects(el: Elt): Set[ObjectLabel] =
    ExecutionContextLattice.getVariableObjects(getExecutionContext(el))
  
  /* Setters */
    
  def setExecutionContext(el: Elt, executionContexts: ExecutionContextLattice.Elt): Elt =
    (getStackFrame(el), executionContexts)
  
  def updateStackFrame(el: Elt, register: Int, value: ValueLattice.Elt, strong: Boolean = false): Elt =
    (StackFrameLattice.updateRegisterValue(getStackFrame(el), register, value, strong), getExecutionContext(el))
}
