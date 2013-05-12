package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object StateLattice extends ProductLattice(HeapLattice, StackLattice) {
  /* Getters */
  
	def getHeap(el: Elt) : HeapLattice.Elt = {
		val (heap,_) = el
		heap
	}

	def getStack(el: Elt) : StackLattice.Elt = {
		val (_,stack) = el
		stack
	}
	
	def getHeapObject(el: Elt, label: ObjectLabel): ObjectLattice.Elt = {
	  HeapLattice.getObject(getHeap(el), label)
	}
	
	def getVariableObjects(el: Elt): Set[ObjectLabel] =
	  StackLattice.getVariableObject(getStack(el))
	
	/* Setters */
	
	def setStack(el: Elt, stack: StackLattice.Elt): Elt =
	  (getHeap(el), stack)
	
	def setVariableObject(el: Elt, label: ObjectLabel): Elt =
	  (getHeap(el), StackLattice.setVariableObject(getStack(el), label))
	
	def setExecutionContext(el: Elt, executionContext: ExecutionContextLattice.Elt): Elt =
	  (getHeap(el), StackLattice.setExecutionContext(getStack(el), executionContext))
	  
	/* Updaters */
	  
	def updateStackFrame(el: Elt, register: Int, value: ValueLattice.Elt): Elt =
	  (getHeap(el), StackLattice.updateStackFrame(getStack(el), register, value))
	
	def updateHeap(el: Elt, label: ObjectLabel, obj: ObjectLattice.Elt): Elt =
	  (HeapLattice.updateHeap(getHeap(el), label, obj), getStack(el))
}