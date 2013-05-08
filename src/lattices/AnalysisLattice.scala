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

  /* Pack and unpack */
  
  def unpackElement(node: Node, currentSolution: AnalysisLattice.Elt): (CallGraphLattice.Elt, HeapLattice.Elt, StackFrameLattice.Elt, ExecutionContextLattice.Elt) = {
    val (programState, callGraph) = currentSolution
    val state = ProgramStateLattice.get(programState, node)
    val (heap, (stack, executionContext)) = state
    (callGraph, heap, stack, executionContext)
  }
   
  
  def packElement(node: Node, currentSolution: AnalysisLattice.Elt, callGraph: CallGraphLattice.Elt, heap: HeapLattice.Elt, stack: StackFrameLattice.Elt, executionContext: ExecutionContextLattice.Elt): Elt = {
    var (programState,_) = currentSolution
    programState = programState + (node -> (heap,(stack,executionContext)))
    return (programState, callGraph)
  }
}