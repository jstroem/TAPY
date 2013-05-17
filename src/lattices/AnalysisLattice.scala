package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object ProgramStateLattice extends MapLattice[Node, StateLattice.Elt](StateLattice) {
  
  /* Getters */
  
  def getState(el: ProgramStateLattice.Elt, node: Node): StateLattice.Elt =
    get(el, node)
  
  def getVariableObjects(el: ProgramStateLattice.Elt, node: Node): Set[ObjectLabel] =
    StateLattice.getVariableObjects(getState(el, node))
    
  /* Setters */
  
  def setExecutionContext(el: ProgramStateLattice.Elt, node: Node, executionContext: ExecutionContextLattice.Elt): ProgramStateLattice.Elt =
    update(el, node, StateLattice.setExecutionContext(getState(el, node), executionContext))
}

object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node)] { // Any: Context sensitivity

  /* Getters */
  
  def getPredecessors(el: CallGraphLattice.Elt, node: Node): Set[Node] =
    if (el != null)
      el.foldLeft(Set[Node]()) {(acc, elt) =>
        val (_, pred, _, succ) = elt
        if (succ == node) acc + pred else acc
      }
    else
      Set()
  
  def getSuccessors(el: CallGraphLattice.Elt, node: Node): Set[Node] =
    if (el != null)
      el.foldLeft(Set[Node]()) {(acc, elt) =>
        val (_, pred, _, succ) = elt
        if (pred == node) acc + succ else acc
      }
    else
      Set()
}
object AnalysisLattice extends ProductLattice(ProgramStateLattice, CallGraphLattice) {
    
  /* Getters */
  
  def getCallGraph(el: AnalysisLattice.Elt): CallGraphLattice.Elt = {
    val (_, callGraph) = el
    callGraph
  }

  def getProgramState(el: AnalysisLattice.Elt): ProgramStateLattice.Elt = {
    val (programState, _) = el
    programState    
  }
  
  /* External getters */
  
  def getState(node: Node, el: AnalysisLattice.Elt): StateLattice.Elt = ProgramStateLattice.get(getProgramState(el), node)
  def getStack(node: Node, el: AnalysisLattice.Elt): StackLattice.Elt = StateLattice.getStack(getState(node, el))
  def getHeap(node: Node, el: AnalysisLattice.Elt): HeapLattice.Elt = StateLattice.getHeap(getState(node, el))
  def getHeapObject(node: Node, label: ObjectLabel, el: AnalysisLattice.Elt): ObjectLattice.Elt = StateLattice.getHeapObject(getState(node, el), label)
  def getStackFrame(node: Node, el: AnalysisLattice.Elt): StackFrameLattice.Elt = StackLattice.getStackFrame(getStack(node,el))
  def getExecutionContexts(node: Node, el: AnalysisLattice.Elt): ExecutionContextLattice.Elt = StackLattice.getExecutionContext(getStack(node,el))
  def getVariableObjects(el: AnalysisLattice.Elt, node: Node): Set[ObjectLabel] = ProgramStateLattice.getVariableObjects(getProgramState(el), node)
  
  /* Setters */
  
  def setState(el: AnalysisLattice.Elt, node: Node, state: StateLattice.Elt = StateLattice.bottom): AnalysisLattice.Elt =
    (ProgramStateLattice.update(getProgramState(el), node, state), getCallGraph(el))
  
  def setCallGraph(el: AnalysisLattice.Elt, callGraph: CallGraphLattice.Elt): AnalysisLattice.Elt =
    (getProgramState(el), callGraph)
  
  def setExecutionContexts(el: AnalysisLattice.Elt, node: Node, executionContexts: ExecutionContextLattice.Elt): AnalysisLattice.Elt =
    (ProgramStateLattice.setExecutionContext(getProgramState(el), node, executionContexts), getCallGraph(el))
    
  /* Updaters */
  
  def updateStackFrame(el: AnalysisLattice.Elt, node: Node, register: Int, value: ValueLattice.Elt): AnalysisLattice.Elt = {
    val oldValue = StackFrameLattice.getRegisterValue(getStackFrame(node, el), register)
    setState(el, node, StateLattice.updateStackFrame(getState(node, el), register, ValueLattice.leastUpperBound(value, oldValue)))
  }
  
  def updateHeap(el: AnalysisLattice.Elt, node: Node, label: ObjectLabel, obj: ObjectLattice.Elt): AnalysisLattice.Elt =
    setState(el, node, StateLattice.updateHeap(getState(node, el), label, obj))

  /* Pack and unpack */
  
  def packElement(node: Node, el: AnalysisLattice.Elt, callGraph: CallGraphLattice.Elt, heap: HeapLattice.Elt, stack: StackFrameLattice.Elt, executionContext: ExecutionContextLattice.Elt): Elt = {
    var (programState,_) = el
    programState = ProgramStateLattice.update(programState, node, (heap, (stack,executionContext)))
    return (programState, callGraph)
  }

  def unpackElement(node: Node, el: AnalysisLattice.Elt): (CallGraphLattice.Elt, HeapLattice.Elt, StackFrameLattice.Elt, ExecutionContextLattice.Elt) = {
    val (programState, callGraph) = el
    val state = ProgramStateLattice.get(programState, node)
    val (heap, (stack, executionContext)) = state
    (callGraph, heap, stack, executionContext)
  }
}
