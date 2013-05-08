package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object ProgramStateLattice extends MapLattice[Node, StateLattice.Elt](StateLattice)
object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node)] // Any: Context sensitivity



object AnalysisLattice extends ProductLattice(
  ProgramStateLattice,
  CallGraphLattice) {

  /* Put element */
  
  def putElement(analysis: AnalysisLattice.Elt, node: Node, state: StateLattice.Elt): AnalysisLattice.Elt = {
    val (programState, callGraph) = analysis
    return (programState + (node -> state), callGraph)
  }
  
  def putElement(analysis: AnalysisLattice.Elt, node: Node, callGraph: CallGraphLattice.Elt): AnalysisLattice.Elt = {
    val (programState, _) = analysis
    return (programState, callGraph)
  }

  def unpackElement(node: Node, el: AnalysisLattice.Elt) : (CallGraphLattice.Elt, HeapLattice.Elt, StackFrameLattice.Elt, ExecutionContextLattice.Elt) = {
    val (programState, callGraph) = el
    val state = ProgramStateLattice.get(programState, node)
    val (heap, (stack, executionContext)) = state
    (callGraph, heap, stack, executionContext)
  }

  def getCallGraph(el: AnalysisLattice.Elt) : CallGraphLattice.Elt = {
  	val (_, callGraph) = el
  	callGraph
  }

  def getProgramState(el: AnalysisLattice.Elt) : ProgramStateLattice.Elt = {
	val (programState, _) = el
	programState  	
  }


  def getState(node: Node, el: AnalysisLattice.Elt) : StateLattice.Elt = ProgramStateLattice.get(getProgramState(el), node)
  def getStack(node: Node, el: AnalysisLattice.Elt) : StackLattice.Elt = StateLattice.getStack(getState(node, el))
  def getHeap(node: Node, el: AnalysisLattice.Elt) : HeapLattice.Elt = StateLattice.getHeap(getState(node, el))
  def getStackFrame(node: Node, el: AnalysisLattice.Elt) : StackFrameLattice.Elt = StackLattice.getStackFrame(getStack(node,el))
  def getExecutionContext(node: Node, el: AnalysisLattice.Elt) : ExecutionContextLattice.Elt = StackLattice.getExecutionContext(getStack(node,el))

  def packElement(node: Node, el : AnalysisLattice.Elt, callGraph: CallGraphLattice.Elt, heap: HeapLattice.Elt, stack: StackFrameLattice.Elt, executionContext: ExecutionContextLattice.Elt) : Elt = {
    var (programState,_) = el
    programState = programState + (node -> (heap,(stack,executionContext)))
    return (programState, callGraph)
  }
}