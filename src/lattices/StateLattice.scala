package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object HeapLattice extends MapLattice[String, ObjectLattice.Elt](ObjectLattice)

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
	
	/* Setters */
	
	def setStack(el: StateLattice.Elt, stack: StackLattice.Elt): StateLattice.Elt =
	  (getHeap(el), stack)
	
	def updateStackFrame(el: StateLattice.Elt, register: Int, value: ValueLattice.Elt): StateLattice.Elt =
	  (getHeap(el), StackLattice.updateStackFrame(getStack(el), register, value))
}