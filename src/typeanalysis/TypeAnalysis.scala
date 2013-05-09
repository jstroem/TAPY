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
        
        case node: WriteVariableNode => ((solution) => handleWriteVariableNode(node, join(node, solution)))
        
        case node => ((solution) => join(node, solution))
      }
  }

  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getSuccessors(cfgNode)
  }
  
  def join(node: Node, solution: Elt): Elt = {
    val state = cfg.getPredecessors(node).foldLeft(StateLattice.bottom)((acc, pred) => 
      StateLattice.leastUpperBound(acc, AnalysisLattice.getState(pred, solution)))
    AnalysisLattice.setState(solution, node, state)
  }

  /* Misc */
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    /* Create the main module object */
    val obj = ObjectLattice.bottom
    AnalysisLattice.updateHeap(solution, node, "__main__", obj)
  }

  /*
   * Constants: For each constant we can make a a strong update of the register, because there is
   * a unique register for each register index.
   */
  
  def handleConstantBoolean(node: ConstantBooleanNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, node.bool)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantInt(node: ConstantIntNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, node.int.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantFloat(node: ConstantFloatNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, node.float.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantLong(node: ConstantLongNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, node.long.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantComplex(node: ConstantComplexNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, node.complex.getReal().getValue(), node.complex.getImag().getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantString(node: ConstantStringNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, node.string)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantNone(node: ConstantNoneNode, solution: Elt): Elt = {
    val value = ValueLattice.putElement(ValueLattice.bottom, NoneLattice.top)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  /* Variables */
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.valueReg)
    
    val oldScopeObj = HeapLattice.getHeapObject(AnalysisLattice.getHeap(node, solution), "__main__")
    val newScopeObj = ObjectLattice.updatePropertyValue(oldScopeObj, node.variable, value)
    
    AnalysisLattice.updateHeap(solution, node, "__main__", newScopeObj)
  }
}