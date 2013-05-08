package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object HeapLattice extends MapLattice[String, ObjectLattice.Elt](ObjectLattice)

object TAJSUnkownLattice extends ProductLattice(new PowerSubSetLattice[String](),new PowerSubSetLattice[String]()) //Todo: In TAJS they have to Powersets in their StateLattice

object StateLattice extends ProductLattice(HeapLattice,StackLattice) {

	def getHeap(el: StateLattice.Elt) : HeapLattice.Elt = {
		val (heap,_) = el
		heap
	}

	def getStack(el: StateLattice.Elt) : StackLattice.Elt = {
		val (_,stack) = el
		stack
	}
}