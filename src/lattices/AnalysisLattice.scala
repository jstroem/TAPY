package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object AnalysisLattice extends ProductLattice(ProgramStateLattice, CallGraphLattice) {
    
  /* Getters */
  
  def getCallGraph(el: Elt): CallGraphLattice.Elt = {
    val (_, callGraph) = el
    callGraph
  }

  def getProgramState(el: Elt): ProgramStateLattice.Elt = {
    val (programState, _) = el
    programState    
  }
  
  /* External getters */
  
  def getState(node: Node, el: Elt): StateLattice.Elt = ProgramStateLattice.get(getProgramState(el), node)
  def getStack(node: Node, el: Elt): StackLattice.Elt = StateLattice.getStack(getState(node, el))
  def getHeap(node: Node, el: Elt): HeapLattice.Elt = StateLattice.getHeap(getState(node, el))
  def getHeapObject(node: Node, label: ObjectLabel, el: Elt): ObjectLattice.Elt = StateLattice.getHeapObject(getState(node, el), label)
  def getStackFrame(node: Node, el: Elt): StackFrameLattice.Elt = StackLattice.getStackFrame(getStack(node,el))
  def getExecutionContexts(node: Node, el: Elt): ExecutionContextLattice.Elt = StackLattice.getExecutionContext(getStack(node,el))
  def getVariableObjects(el: Elt, node: Node): Set[ObjectLabel] = ProgramStateLattice.getVariableObjects(getProgramState(el), node)
  
  /* Setters */
  
  def setState(el: Elt, node: Node, state: StateLattice.Elt = StateLattice.bottom): Elt =
    (ProgramStateLattice.update(getProgramState(el), node, state), getCallGraph(el))
  
  def setCallGraph(el: Elt, callGraph: CallGraphLattice.Elt): Elt =
    (getProgramState(el), callGraph)
  
  def setExecutionContexts(el: Elt, node: Node, executionContexts: ExecutionContextLattice.Elt = ExecutionContextLattice.bottom): Elt =
    (ProgramStateLattice.setExecutionContext(getProgramState(el), node, executionContexts), getCallGraph(el))
    
  /* Updaters */
  
  def updateStackFrame(el: Elt, node: Node, register: Int, value: ValueLattice.Elt, strong: Boolean = false): Elt =
    setState(el, node, StateLattice.updateStackFrame(getState(node, el), register, value, strong))
  
  def updateHeap(el: Elt, node: Node, label: ObjectLabel, obj: ObjectLattice.Elt): Elt =
    setState(el, node, StateLattice.updateHeap(getState(node, el), label, obj))
  
  def updateCallGraph(el: Elt, callGraph: CallGraphLattice.Elt): Elt =
    (getProgramState(el), CallGraphLattice.leastUpperBound(getCallGraph(el), callGraph))

  /* Pack and unpack */
  
  def packElement(node: Node, el: Elt, callGraph: CallGraphLattice.Elt, heap: HeapLattice.Elt, stack: StackFrameLattice.Elt, executionContext: ExecutionContextLattice.Elt): Elt = {
    var (programState,_) = el
    programState = ProgramStateLattice.update(programState, node, (heap, (stack,executionContext)))
    return (programState, callGraph)
  }

  def unpackElement(node: Node, el: Elt): (CallGraphLattice.Elt, HeapLattice.Elt, StackFrameLattice.Elt, ExecutionContextLattice.Elt) = {
    val (programState, callGraph) = el
    val state = ProgramStateLattice.get(programState, node)
    val (heap, (stack, executionContext)) = state
    (callGraph, heap, stack, executionContext)
  }
}
