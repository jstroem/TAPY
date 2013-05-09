package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object StackFrameLattice extends MapLattice[Int, ValueLattice.Elt](ValueLattice) {
  /* Getters */
  
  def getRegisterValue(el: StackFrameLattice.Elt, register: Int): ValueLattice.Elt =
    el.getOrElse(register, ValueLattice.bottom)
}

object ExecutionContextLattice extends PowerSubSetLattice[(List[String], String)] {
  
  /* Constructors */
  
  def makeElement(scopeChain: List[String], variableObject: String): ExecutionContextLattice.Elt =
    Set((scopeChain, variableObject))
  
  /* Getters */
    
  def getVariableObject(el: ExecutionContextLattice.Elt): Set[String] =
    el.map({ case (_, variableObject) => variableObject })
    
  /* Setters */
  
  def setVariableObject(el: ExecutionContextLattice.Elt, label: String): ExecutionContextLattice.Elt =
    throw new NotImplementedException()
}

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
	
	def getVariableObject(el: StackLattice.Elt): Set[String] =
	  ExecutionContextLattice.getVariableObject(getExecutionContext(el))
	
	/* Setters */
	
	def setExecutionContext(el: StackLattice.Elt, executionContext: ExecutionContextLattice.Elt): StackLattice.Elt =
	  (getStackFrame(el), executionContext)
	
	def updateStackFrame(el: StackLattice.Elt, register: Int, value: ValueLattice.Elt): StackLattice.Elt =
	  (getStackFrame(el) + (register -> value), getExecutionContext(el))
	
	def setVariableObject(el: StackLattice.Elt, label: String): StackLattice.Elt =
	  (getStackFrame(el), ExecutionContextLattice.setVariableObject(getExecutionContext(el), label))
}