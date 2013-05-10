package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object HeapLattice extends MapLattice[ObjectLabel, ObjectLattice.Elt](ObjectLattice) {
  /* Getters */
  
  def getHeapObject(el: HeapLattice.Elt, label: ObjectLabel): ObjectLattice.Elt =
    el.getOrElse(label, ObjectLattice.bottom)
  
  /* Updaters */
  
  def updateHeap(el: HeapLattice.Elt, label: ObjectLabel, obj: ObjectLattice.Elt): HeapLattice.Elt =
    el + (label -> obj)
}