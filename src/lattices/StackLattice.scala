package tapy.lattices

import tapy.dfa._

object StackFrameLattice extends MapLattice[Int, ValueLattice.Elt](ValueLattice)

object ExecutionContextLattice extends PowerSubSetLattice[(List[String], String, String)]

object StackLattice extends ProductLattice(StackFrameLattice,ExecutionContextLattice) {

	def getStackFrame(el: StackLattice.Elt) : StackFrameLattice.Elt = {
		val (stackframe,_) = el
		return stackframe
	}

	def getExecutionContext(el: StackLattice.Elt) : ExecutionContextLattice.Elt = {
		val (_,executionContext) = el
		return executionContext
	}
	
	/* Setters */
	
	def updateStackFrame(el: StackLattice.Elt, register: Int, value: ValueLattice.Elt): StackLattice.Elt =
	  (getStackFrame(el) + (register -> value), getExecutionContext(el))
}