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
        case node: ConstantBooleanNode => ((solution) => handleConstantBoolean(node, solution))
        case node: ConstantIntNode => ((solution) => handleConstantInt(node, solution))
        case node: ConstantFloatNode => ((solution) => handleConstantFloat(node, solution))
        case node: ConstantLongNode => ((solution) => handleConstantLong(node, solution))
        case node: ConstantComplexNode => ((solution) => handleConstantComplex(node, solution))
        case node: ConstantStringNode => ((solution) => handleConstantString(node, solution))
        case node: ConstantNoneNode => ((solution) => handleConstantNone(node, solution))
        case node => ((solution) => solution)
      }
  }

  def handleConstantBoolean(node: ConstantBooleanNode, currentSolution: Elt): Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.bool)
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantInt(node: ConstantIntNode, currentSolution: Elt): Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.int.getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantFloat(node: ConstantFloatNode, currentSolution: Elt) : Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.float.getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantLong(node: ConstantLongNode, currentSolution: Elt) : Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.long.getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantComplex(node: ConstantComplexNode, currentSolution: Elt) : Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.complex.getReal().getValue(), node.complex.getImag().getValue())
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantString(node: ConstantStringNode, currentSolution: Elt): Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, node.string)
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }

  def handleConstantNone(node: ConstantNoneNode, currentSolution: Elt) : Elt = {
    var (callGraph,heap,stack,executionContext) = AnalysisLattice.unpackElement(node, currentSolution)
    var value = ValueLattice.putElement(ValueLattice.bottom, NoneLattice.top)
    stack = stack + (node.resultReg -> value)
    return AnalysisLattice.packElement(node, currentSolution, callGraph, heap, stack, executionContext)
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getPredecessors(cfgNode)
  }
}