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
  
  var callGraph: CallGraphLattice.Elt = null
  var executionContext: ExecutionContextLattice.Elt = null
  var heap: HeapLattice.Elt = null
  var stack: StackLattice.Elt = null
  var stackFrame: StackFrameLattice.Elt = null
  var state: StateLattice.Elt = null
  
  var objectLabelsSeen = Set[ObjectLabel]() // Used to make the top element of the Value lattice
  
  /* Analysis interface */
  
  def generateConstraint(node: Node): Constraint[Elt] = {
    return node match {
        case node: ModuleEntryNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleModuleEntry(node, join)}
        
        case node: ConstantBooleanNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantBoolean(node, join)}
        case node: ConstantIntNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantInt(node, join)}
        case node: ConstantFloatNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantFloat(node, join)}
        case node: ConstantLongNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantLong(node, join)}
        case node: ConstantComplexNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantComplex(node, join)}
        case node: ConstantStringNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantString(node, join)}
        case node: ConstantNoneNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleConstantNone(node, join)}
        
        case node: ReadVariableNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleReadVariableNode(node, join)}
        case node: WriteVariableNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleWriteVariableNode(node, join)}
        
        case node: CompareOpNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleCompareOpNode(node, join)}
        case node: BinOpNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleBinOpNode(node, join)}
        case node: UnOpNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleUnOpNode(node, join)}
        
        case node: FunctionDeclNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleFunctionDeclNode(node, join)}
        case node: FunctionEntryNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleFunctionEntryNode(node, join)}
        case node: CallNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleCallNode(node, join)}
        
        case node => {(solution) => joinPredecessors(node, solution) }
      }
  }
  
  def init(node: Node, solution: Elt): Unit = {
    this.callGraph = AnalysisLattice.getCallGraph(solution)
    this.executionContext = AnalysisLattice.getExecutionContext(node, solution)
    this.heap = AnalysisLattice.getHeap(node, solution)
    this.stack = AnalysisLattice.getStack(node, solution)
    this.stackFrame = AnalysisLattice.getStackFrame(node, solution)
    this.state = AnalysisLattice.getState(node, solution)
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    val callGraphSuccessors = this.callGraph.foldLeft(Set[Node]()) {(acc, elt) =>
      val (_, pred, _, succ) = elt
      if (pred == cfgNode) acc + succ else acc
    }
    return cfg.getSuccessors(cfgNode)
  }
  
  def joinPredecessors(node: Node, solution: Elt): Elt = {
    // Don't use nodeDependencies: this would join state from other functions into call nodes
    val state = cfg.getPredecessors(node).foldLeft(StateLattice.bottom)((acc, pred) => 
      StateLattice.leastUpperBound(acc, AnalysisLattice.getState(pred, solution)))
    AnalysisLattice.setState(solution, node, state)
  }

  /**
    * Utility functions
    */
  
  def findPropertyInScope(node: Node, property: String, solution: Elt): ValueLattice.Elt = {
    val chains = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContext)
    
    chains.foldLeft(ValueLattice.bottom) {(acc, chain) =>
      val value = chain.foldLeft(ValueLattice.bottom) {(acc, objectLabel) =>
        if (acc != ValueLattice.bottom)
          acc
        else
          ObjectPropertyLattice.getValue(ObjectLattice.getProperty(AnalysisLattice.getHeapObject(node, objectLabel, solution), property))
      }
      ValueLattice.leastUpperBound(value, acc)
    }
  }
  
  def writePropertyOnVariableObjects(node: Node, property: String, value: ValueLattice.Elt, solution: Elt): Elt = {
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    variableObjects.foldLeft(solution) {(acc, variableObjectLabel) =>
      val currentVariableObject = HeapLattice.getObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val newVariableObject = ObjectLattice.updatePropertyValue(currentVariableObject, property, value)
      AnalysisLattice.updateHeap(acc, node, variableObjectLabel, newVariableObject)
    }
  }
  
  /* Misc */
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    /* Create the main module object */
    
    val moduleObjectLabel = ScopeObjectLabel("__main__")
    AnalysisLattice.setExecutionContext(
      AnalysisLattice.updateHeap(solution, node, moduleObjectLabel, ObjectLattice.bottom),
      node,
      Set((List(), moduleObjectLabel)))
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
    // Todo use lookup
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    val value = variableObjects.foldLeft(ValueLattice.bottom) {(acc, variableObjectLabel) =>
      val variableObject = HeapLattice.getObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val value = ObjectPropertyLattice.getValue(ObjectLattice.getProperty(variableObject, node.variable))
      ValueLattice.leastUpperBound(value, acc)
    }
    
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value: ValueLattice.Elt = StackFrameLattice.getRegisterValue(this.stackFrame, node.valueReg)
    writePropertyOnVariableObjects(node, node.variable, value, solution)
  }
  
  /* Operators */
  
  /** CompareOpNode: 
    On the operators != and == the result of the comparison is found if the element is either Booleans, Long, String, Float, Integer, Complex or 1 Allocation
    On the operators < <= > and >= the result of the comparison is found if the element is either Booleans, Long, String, Float, Integer
  **/
  def handleCompareOpNode(node: CompareOpNode, solution: Elt): Elt = {
    val left: ValueLattice.Elt = StackFrameLattice.getRegisterValue(this.stackFrame, node.arg1Reg)
    val right: ValueLattice.Elt = StackFrameLattice.getRegisterValue(this.stackFrame, node.arg2Reg)

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
    
    val el1 = StackFrameLattice.getRegisterValue(this.stackFrame, node.arg1Reg)
    val el2 = StackFrameLattice.getRegisterValue(this.stackFrame, node.arg2Reg)
    
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
        case e: ArithmeticException => // TODO: Division by zero
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
    solution
  }
  
  /**
    * Functions 
    */
  
  def handleFunctionDeclNode(node: FunctionDeclNode, solution: Elt): Elt = {
    val functionName = node.entry.funcDef.getInternalName()
    
    // Create labels
    val functionScopeObjectLabel = ScopeObjectLabel(functionName)
    val functionFunctionObjectLabel = FunctionObjectLabel(node.entry, functionScopeObjectLabel)
    val functionObjectLabel = ObjectObjectLabel(functionName)
    
    // Create value lattice elements
    val functionFunctionObjectCallValue = ValueLattice.setObjectLabels(ValueLattice.bottom, Set(functionFunctionObjectLabel))
    val functionObjectValue = ValueLattice.setObjectLabels(ValueLattice.bottom, Set(functionObjectLabel))

    
    // Generate scope-object scope chain
    val functionScopeObjectScopeChain = this.executionContext.foldLeft(Set[List[ObjectLabel]]()) {(acc, pair) =>
      val (scopeChain, variableObject) = pair
      acc + (variableObject :: scopeChain)
    }
    
    // Create objects
    var functionScopeObject = ObjectLattice.setScopeChain(ObjectLattice.bottom, functionScopeObjectScopeChain)
    var functionFunctionObject = ObjectLattice.updatePropertyValue(ObjectLattice.bottom, "__call__", functionFunctionObjectCallValue)
    var functionObject = ObjectLattice.updatePropertyValue(ObjectLattice.bottom, "__call__", functionFunctionObjectCallValue)
    
    // Update the lattice
    var result = AnalysisLattice.updateHeap(solution, node, functionScopeObjectLabel, functionScopeObject)
    result = AnalysisLattice.updateHeap(result, node, functionFunctionObjectLabel, functionFunctionObject)
    result = AnalysisLattice.updateHeap(result, node, functionObjectLabel, functionObject)

    // Add the function name to the current object variables, such that it can be referenced
    writePropertyOnVariableObjects(node, functionName, functionObjectValue, result)
  }
  
  def handleFunctionEntryNode(node: FunctionEntryNode, solution: Elt): Elt = {
    solution
  }
  
  def handleCallNode(node: CallNode, solution: Elt): Elt = {
    try {
      val function: ValueLattice.Elt = StackFrameLattice.getRegisterValue(this.stackFrame, node.functionReg)
      
      if (function == ValueLattice.bottom) {
        // TypeError: Potentially trying to call a non-object
        throw new NotImplementedException()
        
      } else if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](function)) {
        // TypeError: Potentially trying to call a non-object
        throw new NotImplementedException()
        
      } else if (!ValueLattice.elementIsOnlyObjectLabels[CallableObjectLabel](function)) {
        // TypeError: Potentially trying to call a non-function object
        throw new NotImplementedException()
        
      } else {
        // Update the call graph accordingly
        val objLabels = ValueLattice.getObjectLabels(function)
        val callGraph = objLabels.foldLeft(this.callGraph) {(acc, objLabel) =>
          val obj = HeapLattice.getObject(this.heap, objLabel)
          
          val result: CallGraphLattice.Elt =
            if (objLabel.isInstanceOf[ObjectObjectLabel]) {
              handleObjectCall(node, objLabel.asInstanceOf[ObjectObjectLabel], obj, solution)
              
            } else if (objLabel.isInstanceOf[FunctionObjectLabel]) {
              handleFunctionObjectCall(node, objLabel.asInstanceOf[FunctionObjectLabel], obj, solution)
              
            } else {
              // Does not occur: elements has been checked to be CallableObjectLabels
              throw new InternalError()
            }
          
          CallGraphLattice.leastUpperBound(result, acc)
        }
        
        return AnalysisLattice.setCallGraph(solution, callGraph)
      }
    } catch {
      case e: NotImplementedException => e.printStackTrace(); AnalysisLattice.setState(solution, node, StateLattice.top)
    }
  }
  
  def handleObjectCall(node: CallNode, objLabel: ObjectObjectLabel, obj: ObjectLattice.Elt, solution: Elt): CallGraphLattice.Elt = {
    // Check if this is the object of a function
    val call = ObjectLattice.getProperty(obj, "__call__")
    val callValue = ObjectPropertyLattice.getValue(call)
    
    if (call == ObjectPropertyLattice.bottom) {
      // TypeError: Trying to call a non-function object
      throw new NotImplementedException()
      
    } else if (!ValueLattice.elementIsOnlyObjectLabels[FunctionObjectLabel](callValue)) {
      // TypeError: Trying to call a non-function object
      throw new NotImplementedException()
      
    } else {
      val callObjLabels = ValueLattice.getObjectLabels(callValue)
      callObjLabels.foldLeft(CallGraphLattice.bottom) {(acc, callObjLabel) =>
        val callObj = HeapLattice.getObject(this.heap, callObjLabel)
        
        if (callObjLabel.isInstanceOf[FunctionObjectLabel]) {
            handleFunctionObjectCall(node, callObjLabel.asInstanceOf[FunctionObjectLabel], callObj, solution)
          
        } else {
          // TypeError: Trying to call a non-function object
          // Could it be the case that __call__ was set to a non-function object, which has __call__ to a function object?
          throw new NotImplementedException()
        }
      }
    }
  }
  
  def handleFunctionObjectCall(node: CallNode, objLabel: FunctionObjectLabel, obj: ObjectLattice.Elt, solution: Elt): CallGraphLattice.Elt = {
    Set((null, node, null, objLabel.functionEntryNode))
  }
}