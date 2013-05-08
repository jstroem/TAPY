package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object ProgramStateLattice extends MapLattice[Node, StateLattice.Elt](StateLattice)
object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node)] // Any: Context sensitivity



object AnalysisLattice extends ProductLattice(
  ProgramStateLattice,
  CallGraphLattice) {


  def unpackElement(node: Node, el: AnalysisLattice.Elt) : (CallGraphLattice.Elt, HeapLattice.Elt, StackFrameLattice.Elt, ExecutionContextLattice.Elt) = {
    val (programState, callGraph) = el
    val state = ProgramStateLattice.get(programState, node)
    val (heap, (stack,executionContext)) = state
    (callGraph, heap, stack, executionContext)
  }

  def getProgramState(el: AnalysisLattice.Elt) : ProgramStateLattice.Elt = {
	val (programState, callGraph) = el
	programState  	
  }


  def getState(node: Node, el: AnalysisLattice.Elt) : StateLattice.Elt = ProgramStateLattice.get(getProgramState(el), node)

  def packElement(node: Node, el : AnalysisLattice.Elt, callGraph: CallGraphLattice.Elt, heap: HeapLattice.Elt, stack: StackFrameLattice.Elt, executionContext: ExecutionContextLattice.Elt) : Elt = {
    var (programState,_) = el
    programState = programState + (node -> (heap,(stack,executionContext)))
    return (programState, callGraph)
  }
}