package tapy.typeanalysis

import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[AnalysisLattice.Elt] {
  type Elt = AnalysisLattice.Elt
  
  def bottom = AnalysisLattice.bottom
  
  def generateConstraint(node: Node): Constraint[Elt] = {
    return node match {
        case node: ModuleEntryNode => ((solution) => handleModuleEntry(node, join(node, solution)))
        case node: ConstantBooleanNode => ((solution) => handleConstantBoolean(node, join(node, solution)))
        case node: ConstantIntNode => ((solution) => handleConstantInt(node, join(node, solution)))
        case node: ConstantFloatNode => ((solution) => handleConstantFloat(node, join(node, solution)))
        case node: ConstantLongNode => ((solution) => handleConstantLong(node, join(node, solution)))
        case node: ConstantComplexNode => ((solution) => handleConstantComplex(node, join(node, solution)))
        case node: ConstantStringNode => ((solution) => handleConstantString(node, join(node, solution)))
        case node: ConstantNoneNode => ((solution) => handleConstantNone(node, join(node, solution)))
        case node => ((solution) => join(node, solution))
      }
  }

  def join(node: Node, currentSolution : Elt) : Elt = {
    val state = cfg.getPredecessors(node).foldLeft(StateLattice.bottom)((acc, pred) => 
      StateLattice.leastUpperBound(acc, AnalysisLattice.getState(pred, currentSolution)))
    AnalysisLattice.setState(currentSolution, node, state)
  }

  def handleModuleEntry(node: ModuleEntryNode, currentSolution: Elt) : Elt = {
    return currentSolution
  }

  def handleConstantBoolean(node: ConstantBooleanNode, currentSolution: Elt): Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.bool)
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantInt(node: ConstantIntNode, currentSolution: Elt): Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.int.getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantFloat(node: ConstantFloatNode, currentSolution: Elt) : Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.float.getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantLong(node: ConstantLongNode, currentSolution: Elt) : Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.long.getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantComplex(node: ConstantComplexNode, currentSolution: Elt) : Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.complex.getReal().getValue(), node.complex.getImag().getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantString(node: ConstantStringNode, currentSolution: Elt): Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.string)
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantNone(node: ConstantNoneNode, currentSolution: Elt) : Elt = {
    var (callGraph, heap, stack, executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, NoneLattice.top)
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getPredecessors(cfgNode)
  }
}