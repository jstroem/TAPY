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
  var executionContexts: ExecutionContextLattice.Elt = null
  var heap: Map[ObjectLabel, ObjectLattice.Elt] = null
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
        case node: ExitNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleExitNode(node, join)}
        case node: CallNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleCallNode(node, join)}
        case node: AfterCallNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleAfterCallNode(node, join)}
        
        case node => {(solution) => joinPredecessors(node, solution) }
      }
  }
  
  def init(node: Node, solution: Elt): Unit = {
    this.callGraph = AnalysisLattice.getCallGraph(solution)
    this.executionContexts = AnalysisLattice.getExecutionContexts(node, solution)
    this.heap = AnalysisLattice.getHeap(node, solution) match {
      case HeapLattice.Top() => null
      case HeapLattice.Concrete(map) => map
      case _ => throw new InternalError()
    }
    this.stack = AnalysisLattice.getStack(node, solution)
    this.stackFrame = AnalysisLattice.getStackFrame(node, solution)
    this.state = AnalysisLattice.getState(node, solution)
  }
  
  def nodeDependencies(node: Node, solution: Elt): Set[Node] = {
    init(node, solution)
    return cfg.getSuccessors(node) ++ CallGraphLattice.getSuccessors(this.callGraph, node)
  }
  
  def joinPredecessors(node: Node, solution: Elt): Elt = {
    val predecessors = cfg.getPredecessors(node) ++ CallGraphLattice.getPredecessors(this.callGraph, node)
    val state = predecessors.foldLeft(StateLattice.bottom)((acc, pred) => 
      StateLattice.leastUpperBound(acc, AnalysisLattice.getState(pred, solution)))
    AnalysisLattice.setState(solution, node, state)
  }

  /**
    * Utility functions
    */
  
  def findPropertyInScope(node: Node, property: String, solution: Elt): ValueLattice.Elt = {
    val chains = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts)
    
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
    
    val moduleObjectLabel = ModuleScopeObjectLabel("__main__")
    AnalysisLattice.setExecutionContexts(
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
    val functionScopeObjectLabel = FunctionScopeObjectLabel(node.entry)
    val functionFunctionObjectLabel = FunctionObjectLabel(node.entry, node.exit, functionScopeObjectLabel)
    val functionObjectLabel = ObjectObjectLabel(functionName)
    
    // Create value lattice elements
    val functionFunctionObjectCallValue = ValueLattice.setObjectLabels(ValueLattice.bottom, Set(functionFunctionObjectLabel))
    val functionObjectValue = ValueLattice.setObjectLabels(ValueLattice.bottom, Set(functionObjectLabel))

    
    // Generate scope-object scope chain
    val functionScopeObjectScopeChain = this.executionContexts.foldLeft(Set[List[ObjectLabel]]()) {(acc, pair) =>
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
    if (this.heap != null) {
      val scopeObjectLabel = this.heap.foldLeft(null: FunctionScopeObjectLabel) {(acc, entry) =>
        if (acc == null) {
          val (objLabel, _) = entry
          if (objLabel.isInstanceOf[FunctionScopeObjectLabel]) {
            val scopeObjectLabel: FunctionScopeObjectLabel = objLabel.asInstanceOf[FunctionScopeObjectLabel]
            if (scopeObjectLabel.functionEntryNode == node) {
              scopeObjectLabel
            } else acc
          } else acc
        } else acc
      }
      
      if (scopeObjectLabel != null) {
        // Take the current variable object and append it to the scope chain, and
        // set the variable object to the function scope object, such that
        // local declarations will be written onto that object.
        val executionContexts: Set[(List[ObjectLabel], ObjectLabel)] = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts).map({(scopeChain) => (scopeChain, scopeObjectLabel)})
        return AnalysisLattice.setExecutionContexts(solution, node, executionContexts)
      }
    }
    
    // Give up?
    AnalysisLattice.setState(solution, node, StateLattice.top)
  }
  
  def handleExitNode(node: ExitNode, solution: Elt): Elt = {
    AnalysisLattice.setExecutionContexts(solution, node, ExecutionContextLattice.bottom)
  }
  
  def handleCallNode(node: CallNode, solution: Elt): Elt = {
    val afterCallNode = cfg.getSuccessors(node).head.asInstanceOf[AfterCallNode]
    
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
              handleObjectCall(node, afterCallNode, objLabel.asInstanceOf[ObjectObjectLabel], obj, solution)
              
            } else if (objLabel.isInstanceOf[FunctionObjectLabel]) {
              handleFunctionObjectCall(node, afterCallNode, objLabel.asInstanceOf[FunctionObjectLabel], obj, solution)
              
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
  
  def handleObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, objLabel: ObjectObjectLabel, obj: ObjectLattice.Elt, solution: Elt): CallGraphLattice.Elt = {
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
            handleFunctionObjectCall(callNode, afterCallNode, callObjLabel.asInstanceOf[FunctionObjectLabel], callObj, solution)
          
        } else {
          // TypeError: Trying to call a non-function object
          // Could it be the case that __call__ was set to a non-function object, which has __call__ to a function object?
          throw new NotImplementedException()
        }
      }
    }
  }
  
  def handleFunctionObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, objLabel: FunctionObjectLabel, obj: ObjectLattice.Elt, solution: Elt): CallGraphLattice.Elt = {
    Set[(Any, Node, Any, Node)]((null, callNode, null, objLabel.functionEntryNode), (null, objLabel.functionExitNode, null, afterCallNode))
  }
  
  def handleAfterCallNode(node: AfterCallNode, solution: Elt): Elt = {
    solution
  }
}