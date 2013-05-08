package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object ProgramStateLattice extends MapLattice[Node, StateLattice.Elt](StateLattice)
object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node)] // Any: Context sensitivity



object AnalysisLattice extends ProductLattice(
  ProgramStateLattice,
  CallGraphLattice) {
	

  def unpackElement(node: Node, currentSolution: AnalysisLattice.Elt) : (CallGraphLattice.Elt, HeapLattice.Elt, StackFrameLattice.Elt, ExecutionContextLattice.Elt) = {
    val (programState, callGraph) = currentSolution
    val state = ProgramStateLattice.get(programState, node)
    val (heap, (stack,executionContext)) = state
    (callGraph, heap, stack, executionContext)
  }

  def packElement(node: Node, currentSolution : AnalysisLattice.Elt, callGraph: CallGraphLattice.Elt, heap: HeapLattice.Elt, stack: StackFrameLattice.Elt, executionContext: ExecutionContextLattice.Elt) : Elt = {
    val (programState,_) = currentSolution
    val newProgramState = programState + (node -> (heap,(stack,executionContext)))
    return (newProgramState, callGraph)
  }
}