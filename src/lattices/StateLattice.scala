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
    StackLattice.getVariableObjects(getStack(el))
    
  def getExecutionContexts(el: Elt): ExecutionContextLattice.Elt =
    StackLattice.getExecutionContext(getStack(el))
  
  /* Setters */
  
  def setHeap(el: Elt, heap: HeapLattice.Elt): Elt =
    (heap, getStack(el))
    
  def setStack(el: Elt, stack: StackLattice.Elt): Elt =
    (getHeap(el), stack)
  
  def setExecutionContext(el: Elt, executionContext: ExecutionContextLattice.Elt = ExecutionContextLattice.bottom): Elt =
    (getHeap(el), StackLattice.setExecutionContext(getStack(el), executionContext))
    
  /* Updaters */
    
  def updateStackFrame(el: Elt, register: Int, value: ValueLattice.Elt, strong: Boolean = false): Elt =
    (getHeap(el), StackLattice.updateStackFrame(getStack(el), register, value, strong))
  
  def updateHeap(el: Elt, label: ObjectLabel, obj: ObjectLattice.Elt): Elt =
    (HeapLattice.updateHeap(getHeap(el), label, obj), getStack(el))
}