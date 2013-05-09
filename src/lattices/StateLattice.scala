package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object HeapLattice extends MapLattice[String, ObjectLattice.Elt](ObjectLattice) {
  /* Getters */
  
  def getHeapObject(el: HeapLattice.Elt, label: String): ObjectLattice.Elt =
    el.getOrElse(label, ObjectLattice.bottom)
  
  /* Updaters */
  
  def updateHeap(el: HeapLattice.Elt, label: String, obj: ObjectLattice.Elt): HeapLattice.Elt =
    el + (label -> obj)
}

object TAJSUnkownLattice extends ProductLattice(new PowerSubSetLattice[String](),new PowerSubSetLattice[String]()) //Todo: In TAJS they have to Powersets in their StateLattice

object StateLattice extends ProductLattice(HeapLattice,StackLattice) {
  /* Getters */
  
	def getHeap(el: StateLattice.Elt) : HeapLattice.Elt = {
		val (heap,_) = el
		heap
	}

	def getStack(el: StateLattice.Elt) : StackLattice.Elt = {
		val (_,stack) = el
		stack
	}
	
	def getVariableObjects(el: StateLattice.Elt): Set[String] =
	  StackLattice.getVariableObject(getStack(el))
	
	/* Setters */
	
	def setStack(el: StateLattice.Elt, stack: StackLattice.Elt): StateLattice.Elt =
	  (getHeap(el), stack)
	
	def setVariableObject(el: StateLattice.Elt, label: String): StateLattice.Elt =
	  (getHeap(el), StackLattice.setVariableObject(getStack(el), label))
	
	def setExecutionContext(el: StateLattice.Elt, executionContext: ExecutionContextLattice.Elt): StateLattice.Elt =
	  (getHeap(el), StackLattice.setExecutionContext(getStack(el), executionContext))
	  
	/* Updaters */
	  
	def updateStackFrame(el: StateLattice.Elt, register: Int, value: ValueLattice.Elt): StateLattice.Elt =
	  (getHeap(el), StackLattice.updateStackFrame(getStack(el), register, value))
	
	def updateHeap(el: StateLattice.Elt, label: String, obj: ObjectLattice.Elt): StateLattice.Elt =
	  (HeapLattice.updateHeap(getHeap(el), label, obj), getStack(el))
}