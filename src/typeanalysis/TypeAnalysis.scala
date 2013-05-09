package tapy.typeanalysis

import org.python.antlr.ast.operatorType
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
        
        case node: ReadVariableNode => ((solution) => handleReadVariableNode(node, join(node, solution)))
        case node: WriteVariableNode => ((solution) => handleWriteVariableNode(node, join(node, solution)))
        
        case node: CompareOpNode => ((solution) => handleCompareOpNode(node, join(node, solution)))
        case node: BinOpNode => ((solution) => handleBinOpNode(node, join(node, solution)))
        case node: UnOpNode => ((solution) => handleUnOpNode(node, join(node, solution)))
        
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
    AnalysisLattice.setExecutionContext(
      AnalysisLattice.updateHeap(solution, node, "__main__", ObjectLattice.bottom),
      node,
      ExecutionContextLattice.makeElement(List(), "__main__"))
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
  
  def handleReadVariableNode(node: ReadVariableNode, solution: Elt): Elt = {
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    val value = variableObjects.foldLeft(ValueLattice.bottom) {(acc, variableObjectLabel) =>
      val variableObject = HeapLattice.getHeapObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val value = ObjectPropertyLattice.getValue(ObjectLattice.getObjectProperty(variableObject, node.variable))
      ValueLattice.leastUpperBound(value, acc)
    }
    
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.valueReg)
    
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    variableObjects.foldLeft(solution) {(acc, variableObjectLabel) =>
      val currentVariableObject = HeapLattice.getHeapObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val newVariableObject = ObjectLattice.updatePropertyValue(currentVariableObject, node.variable, value)
      AnalysisLattice.updateHeap(acc, node, variableObjectLabel, newVariableObject)
    }
  }
  
  /* Operators */
  
  def handleCompareOpNode(node: CompareOpNode, solution: Elt): Elt = {
    solution
  }
  
  def handleBinOpNode(node: BinOpNode, solution: Elt): Elt = {
    // See:
    // - http://docs.python.org/2/reference/expressions.html#binary-bitwise-operations
    // - http://docs.python.org/2/reference/expressions.html#binary-arithmetic-operations
    node.op match {
      case operatorType.UNDEFINED => "UNDEFINED"
      case operatorType.Add => "+"
      case operatorType.Sub => "-"
        
      case operatorType.Mult =>
        // The * (multiplication) operator yields the product of its arguments. The arguments must either:
        // - both be numbers, or
        // - one argument must be an integer (plain or long) and the other must be a sequence (UNSUPPORTED).
        // In the former case, the numbers are converted to a common type and then multiplied
        // together. In the latter case, sequence repetition is performed; a negative repetition
        // factor yields an empty sequence.
        val left = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg1Reg)
        val right = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg2Reg)
        
        if (ValueLattice.elementIsNumber(left) && ValueLattice.elementIsNumber(right)) {
          if (ValueLattice.elementIsFloat(left) && ValueLattice.elementIsFloat(right)) {
            
          }
          
          
          
          val (leftInteger, leftFloat, leftLong, leftComplex) = ValueLattice.getElementNumbers(left)
        } else {
          // TODO: TypeError
        }
        "*"
        
      case operatorType.Div => "/"
      case operatorType.Mod => "%"
      case operatorType.Pow => "**"
      case operatorType.LShift => ">>"
      case operatorType.RShift => "<<"
      case operatorType.BitOr => "|"
      case operatorType.BitXor => "^"
      case operatorType.BitAnd => "&"
      case operatorType.FloorDiv => "//"
    }
    solution
  }
  
  def handleUnOpNode(node: UnOpNode, solution: Elt): Elt = {
    solution
  }
}