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

  def getVariableObject(el: Elt): Set[ObjectLabel] =
    ExecutionContextLattice.getVariableObject(getExecutionContext(el))
  
  /* Setters */
  def setExecutionContext(el: Elt, executionContext: ExecutionContextLattice.Elt): Elt =
    (getStackFrame(el), executionContext)
  
  def updateStackFrame(el: Elt, register: Int, value: ValueLattice.Elt): Elt =
    (StackFrameLattice.updateRegisterValue(getStackFrame(el), register, value), getExecutionContext(el))
  
  def setVariableObject(el: Elt, label: ObjectLabel): Elt =
    (getStackFrame(el), ExecutionContextLattice.setVariableObject(getExecutionContext(el), label))
}
