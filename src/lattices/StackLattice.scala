package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object StackLattice extends ProductLattice(StackFrameLattice,ExecutionContextLattice) {
  /* Getters */
  
	def getStackFrame(el: StackLattice.Elt) : StackFrameLattice.Elt = {
		val (stackframe,_) = el
		return stackframe
	}

	def getExecutionContext(el: StackLattice.Elt) : ExecutionContextLattice.Elt = {
		val (_,executionContext) = el
		return executionContext
	}
	
	def getVariableObject(el: StackLattice.Elt): Set[ObjectLabel] =
	  ExecutionContextLattice.getVariableObject(getExecutionContext(el))
	
	/* Setters */
	
	def setExecutionContext(el: StackLattice.Elt, executionContext: ExecutionContextLattice.Elt): StackLattice.Elt =
	  (getStackFrame(el), executionContext)
	
	def updateStackFrame(el: StackLattice.Elt, register: Int, value: ValueLattice.Elt): StackLattice.Elt =
	  (getStackFrame(el) + (register -> value), getExecutionContext(el))
	
	def setVariableObject(el: StackLattice.Elt, label: ObjectLabel): StackLattice.Elt =
	  (getStackFrame(el), ExecutionContextLattice.setVariableObject(getExecutionContext(el), label))
}