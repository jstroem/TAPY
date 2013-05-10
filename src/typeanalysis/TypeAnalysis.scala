package tapy.typeanalysis

import java.lang.ArithmeticException
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[AnalysisLattice.Elt] {
  type Elt = AnalysisLattice.Elt
  
  def bottom = AnalysisLattice.bottom
  
  /* Analysis interface */
  
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
        
        case node: FunctionDeclNode => ((solution) => handleFunctionDeclNode(node, join(node, solution)))
        case node: FunctionEntryNode => ((solution) => handleFunctionEntryNode(node, join(node, solution)))
        case node: CallNode => ((solution) => handleCallNode(node, join(node, solution)))
        
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

  /**
    * Utility functions
    */
  
  def writePropertyOnVariableObjects(node: Node, property: String, value: ValueLattice.Elt, solution: Elt): Elt = {
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    variableObjects.foldLeft(solution) {(acc, variableObjectLabel) =>
      val currentVariableObject = HeapLattice.getHeapObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val newVariableObject = ObjectLattice.updatePropertyValue(currentVariableObject, property, value)
      AnalysisLattice.updateHeap(acc, node, variableObjectLabel, newVariableObject)
    }
  }
  
  /* Misc */
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    /* Create the main module object */
    
    val moduleObjectLabel = ObjectObjectLabel("__main__")
    AnalysisLattice.setExecutionContext(
      AnalysisLattice.updateHeap(solution, node, moduleObjectLabel, ObjectLattice.bottom),
      node,
      ExecutionContextLattice.makeElement(List(), moduleObjectLabel))
  }

  /*
   * Constants: For each constant we can make a a strong update of the register, because there is
   * a unique register for each register index.
   */
  
  def handleConstantBoolean(node: ConstantBooleanNode, solution: Elt): Elt = {
    val value = ValueLattice.setBoolean(ValueLattice.bottom, node.bool)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantInt(node: ConstantIntNode, solution: Elt): Elt = {
    val value = ValueLattice.setInteger(ValueLattice.bottom, node.int.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantFloat(node: ConstantFloatNode, solution: Elt): Elt = {
    val value = ValueLattice.setFloat(ValueLattice.bottom, node.float.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantLong(node: ConstantLongNode, solution: Elt): Elt = {
    val value = ValueLattice.setLong(ValueLattice.bottom, node.long.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantComplex(node: ConstantComplexNode, solution: Elt): Elt = {
    val value = ValueLattice.setComplex(ValueLattice.bottom, node.complex.getReal().getValue(), node.complex.getImag().getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantString(node: ConstantStringNode, solution: Elt): Elt = {
    val value = ValueLattice.setString(ValueLattice.bottom, node.string)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantNone(node: ConstantNoneNode, solution: Elt): Elt = {
    val value = ValueLattice.setNone(ValueLattice.bottom, NoneLattice.top)
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
    val value: ValueLattice.Elt = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.valueReg)
    writePropertyOnVariableObjects(node, node.variable, value, solution)
  }
  
  /* Operators */
  
  /** CompareOpNode: 
    On the operators != and == the result of the comparison is found if the element is either Booleans, Long, String, Float, Integer, Complex or 1 Allocation
    On the operators < <= > and >= the result of the comparison is found if the element is either Booleans, Long, String, Float, Integer
  **/
  def handleCompareOpNode(node: CompareOpNode, solution: Elt): Elt = {
    val left: ValueLattice.Elt = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg1Reg)
    val right: ValueLattice.Elt = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg2Reg)

    val value = ValueLattice.elementCompare(node.op, left, right)

    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  /**
    * If left is exactly one number (including boolean) and right is exactly one number, the result is computed.
    */
  def handleBinOpNode(node: BinOpNode, solution: Elt): Elt = {
    // See:
    // - http://docs.python.org/2/reference/expressions.html#binary-bitwise-operations
    // - http://docs.python.org/2/reference/expressions.html#binary-arithmetic-operations
    
    var value: ValueLattice.Elt = null
    var exception = null
    
    val el1 = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg1Reg)
    val el2 = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg2Reg)
    
    if (ValueLattice.elementIsOnlyNumber(el1) && ValueLattice.elementIsOnlyNumber(el2)) {
      /* Both el1 and el2 are numbers (i.e. either boolean, integer, float, long or complex and NOT e.g. string */
      val (el1Common, el2Common) = ValueLattice.elementsToCommonType(el1, el2)
      
      try {
        if (ValueLattice.elementIsOnlyBoolean(el1Common) && ValueLattice.elementIsOnlyBoolean(el2Common))
          value = BooleanLattice.binaryOperator(ValueLattice.getBoolean(el1Common), ValueLattice.getBoolean(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyInteger(el1Common) && ValueLattice.elementIsOnlyInteger(el2Common))
          value = IntegerLattice.binaryOperator(ValueLattice.getInteger(el1Common), ValueLattice.getInteger(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyFloat(el1Common) && ValueLattice.elementIsOnlyFloat(el2Common))
          value = FloatLattice.binaryOperator(ValueLattice.getFloat(el1Common), ValueLattice.getFloat(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyLong(el1Common) && ValueLattice.elementIsOnlyLong(el2Common))
          value = LongLattice.binaryOperator(ValueLattice.getLong(el1Common), ValueLattice.getLong(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyComplex(el1Common) && ValueLattice.elementIsOnlyComplex(el2Common))
          value = ComplexLattice.binaryOperator(ValueLattice.getComplex(el1Common), ValueLattice.getComplex(el2Common), node.op)
        else
          throw new NotImplementedException()
      } catch {
        case e: ArithmeticException => throw new NotImplementedException() // TODO: Division by zero
      }
    } else if (ValueLattice.elementIsOnlyString(el1) && ValueLattice.elementIsOnlyString(el2)) {
      value = StringLattice.binaryOperator(ValueLattice.getString(el1), ValueLattice.getString(el2), node.op)
    } else {
      // TODO: TypeError
    }
    
    if (value != null)
      AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
    else
      solution
  }
  
  def handleUnOpNode(node: UnOpNode, solution: Elt): Elt = {
    val el1 = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg1Reg)

    var value: ValueLattice.Elt = null

    try {
      if (ValueLattice.elementIsOnlyBoolean(el1))
        value = BooleanLattice.unaryOperator(ValueLattice.getBoolean(el1), node.op)
      else if (ValueLattice.elementIsOnlyInteger(el1))
        value = IntegerLattice.unaryOperator(ValueLattice.getInteger(el1), node.op)
      else if (ValueLattice.elementIsOnlyLong(el1))
        value = LongLattice.unaryOperator(ValueLattice.getLong(el1), node.op)
      else if (ValueLattice.elementIsOnlyFloat(el1))
        value = FloatLattice.unaryOperator(ValueLattice.getFloat(el1), node.op)
      else if (ValueLattice.elementIsOnlyString(el1))
        value = StringLattice.unaryOperator(ValueLattice.getString(el1), node.op)
      else if (ValueLattice.elementIsOnlyComplex(el1))
        value = ComplexLattice.unaryOperator(ValueLattice.getComplex(el1), node.op)
      else if (ValueLattice.elementIsOnlyNone(el1))
        value = NoneLattice.unaryOperator(ValueLattice.getNone(el1), node.op)
      else
        throw new NotImplementedException()
    } catch {
      case e: ArithmeticException => throw new NotImplementedException() //TODO: different errors can happen
    }

    if (value != null)
      AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
    else
      solution
  }
  
  /**
    * Functions 
    */
  
  def handleFunctionDeclNode(node: FunctionDeclNode, solution: Elt): Elt = {
    val functionName = node.entry.funcDef.getInternalName()
    
    // 1) Create function object on heap
    val functionFunctionObjectLabel = FunctionObjectLabel(node.entry)
    val functionFunctionObjectCallValue = ValueLattice.setObjectLabels(ValueLattice.bottom, Set(functionFunctionObjectLabel))
    val functionFunctionObject = ObjectLattice.updatePropertyValue(ObjectLattice.bottom, "__call__", functionFunctionObjectCallValue)
    var result = AnalysisLattice.updateHeap(solution, node, functionFunctionObjectLabel, functionFunctionObject)
    
    // 2) Create object on heap that points to the function with __call__
    val functionObjectLabel = ObjectObjectLabel(functionName)
    val functionObject = ObjectLattice.updatePropertyValue(ObjectLattice.bottom, "__call__", functionFunctionObjectCallValue)
    result = AnalysisLattice.updateHeap(result, node, functionObjectLabel, functionObject)
    
    // 3) Add the object as a property to variable object
    val functionObjectValue = ValueLattice.setObjectLabels(ValueLattice.bottom, Set(functionObjectLabel))
    writePropertyOnVariableObjects(node, functionName, functionObjectValue, result)
  }
  
  def handleFunctionEntryNode(node: FunctionEntryNode, solution: Elt): Elt = {
    solution
  }
  
  def handleCallNode(node: CallNode, solution: Elt): Elt = {
    solution
  }
}