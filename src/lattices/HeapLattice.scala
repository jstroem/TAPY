package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object HeapLattice extends MapLattice[ObjectLabel, ObjectLattice.Elt](ObjectLattice) {

  /* Getters */
  def getHeapObject(el: HeapLattice.Elt, label: ObjectLabel): ObjectLattice.Elt =
    get(el, label)
  
  /* Updaters */  
  def updateHeap(el: HeapLattice.Elt, label: ObjectLabel, obj: ObjectLattice.Elt): HeapLattice.Elt =
    update(el, label, obj)
}
