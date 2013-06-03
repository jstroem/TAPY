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
  
  def updateStackFrames(el: Elt, node: Node, pairs: Set[(Int, ValueLattice.Elt)], strong: Boolean = false): Elt =
    pairs.foldLeft(el) {(acc, pair) =>
      val (register, value) = pair
      updateStackFrame(acc, node, register, value, strong)
    }
  
  def updateHeap(el: Elt, node: Node, label: ObjectLabel, obj: ObjectLattice.Elt): Elt =
    setState(el, node, StateLattice.updateHeap(getState(node, el), label, obj))
  
  def updateHeap(el: Elt, node: Node, pairs: Set[(ObjectLabel, ObjectLattice.Elt)]): Elt =
    pairs.foldLeft(el) {(acc, pair) =>
      val (label, obj) = pair
      setState(acc, node, StateLattice.updateHeap(getState(node, acc), label, obj))
    }
  
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
  
  /* Utilities */
  
  def diff(el1: Elt, el2: Elt, node: Node): String = {
    var result = ""
    
    val (programState1, callGraph1) = el1
    val (programState2, callGraph2) = el2
    
    if (callGraph1 != callGraph2) {
      result = "Call graphs differ: " + (((callGraph1 union callGraph2) intersect callGraph1) intersect callGraph2) + ".\n"
    }
    
    val state1 = node.getState(el1)
    val state2 = node.getState(el2)
    
    val (heap1, stack1) = state1
    val (heap2, stack2) = state2
    
    if (heap1 != heap2) {
      result += "Heaps differ.\n"
    }
    
    if (stack1 != stack2) {
      val (stackFrame1, executionContext1) = stack1
      val (stackFrame2, executionContext2) = stack2
      
      if (stackFrame1 != stackFrame2) {
        result += "Stack frames differ.\n"
        (stackFrame1, stackFrame2) match {
          case (StackFrameLattice.Concrete(stackFrame1), StackFrameLattice.Concrete(stackFrame2)) =>
            val keys1 = stackFrame1.keys
            val keys2 = stackFrame2.keys
            
            if (keys1.toSet == keys2.toSet) {
              result += "Stack frame key sets identical.\n"
              keys1.foreach {(key) =>
                val value1 = stackFrame1.get(key)
                val value2 = stackFrame2.get(key)
                
                if (value1 != value2)
                  result += "Stack frame values for register " + key + " differ:\n- " + value1 + "\n- " + value2 + ".\n"
              }
            }
            
          case _ =>
        }
      }
      
      if (executionContext1 != executionContext2)
        result += "Execution context differ.\n"
    }
    
    return result
  }
}
