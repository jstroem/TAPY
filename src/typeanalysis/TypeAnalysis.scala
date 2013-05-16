package tapy.typeanalysis

import java.lang.ArithmeticException
import org.python.antlr.ast.arguments
import org.python.antlr.ast.Name
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import tapy.exceptions._
import scala.collection.JavaConversions._

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[AnalysisLattice.Elt] {
  type Elt = AnalysisLattice.Elt
  
  def bottom = AnalysisLattice.bottom
  
  var callGraph: CallGraphLattice.Elt = null
  var executionContexts: ExecutionContextLattice.Elt = null
  var heap: Map[ObjectLabel, ObjectLattice.Elt] = null
  var stack: StackLattice.Elt = null
  var stackFrame: StackFrameLattice.Elt = null
  var state: StateLattice.Elt = null
  var environments: Map[Node, Set[String]] = Environment.build(cfg)
  
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
        case node: ReturnNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleReturnNode(node, join)}
        case node: AfterCallNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleAfterCallNode(node, join)}

        case node: GlobalNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleGlobalNode(node, join)}
        case node: ClassDeclNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleClassDeclNode(node, join)}
        case node: ClassEntryNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleClassEntryNode(node, join)}

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
  
  def writePropertyValueOnVariableObjects(node: Node, property: String, value: ValueLattice.Elt, solution: Elt): Elt =
    writePropertyOnVariableObjects(node, property, ObjectPropertyLattice.setValue(ObjectPropertyLattice.bottom, value), solution)
  
  def writePropertyOnVariableObjects(node: Node, property: String, value: ObjectPropertyLattice.Elt, solution: Elt): Elt = {
    val variableObjectLabels = AnalysisLattice.getVariableObjects(solution, node)
    variableObjectLabels.foldLeft(solution) {(acc, variableObjectLabel) =>
      val newVariableObject = writePropertyOnObject(node, variableObjectLabel, property, value, solution)
      AnalysisLattice.updateHeap(acc, node, variableObjectLabel, newVariableObject)
    }
  }
  
  def writePropertyValueOnObject(node: Node, objectLabel: ObjectLabel, property: String, value: ValueLattice.Elt, solution: Elt): ObjectLattice.Elt =
    writePropertyOnObject(node, objectLabel, property, ObjectPropertyLattice.setValue(ObjectPropertyLattice.bottom, value), solution)
  
  def writePropertyOnObject(node: Node, objectLabel: ObjectLabel, property: String, value: ObjectPropertyLattice.Elt, solution: Elt): ObjectLattice.Elt = {
    val currentVariableObject = HeapLattice.getObject(AnalysisLattice.getHeap(node, solution), objectLabel)
    val currentPropertyValue = ObjectLattice.getProperty(currentVariableObject, property)
    val newPropertyValue = ObjectPropertyLattice.leastUpperBound(value, currentPropertyValue)
    ObjectLattice.setProperty(currentVariableObject, property, newPropertyValue)
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
    val value = findPropertyInScope(node, node.variable, solution)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.valueReg)
    writePropertyValueOnVariableObjects(node, node.variable, value, solution)
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
      val (el1s, el2s) = (ValueLattice.splitElement(el1), ValueLattice.splitElement(el2))
      
      value =
        el1s.foldLeft(ValueLattice.bottom) {(acc, el1) =>
          el2s.foldLeft(acc) {(acc, el2) =>
            val (el1Common, el2Common) = ValueLattice.elementsToCommonType(el1, el2)
            try {
              val value =
                if (ValueLattice.elementIsOnlyBoolean(el1Common) && ValueLattice.elementIsOnlyBoolean(el2Common))
                  BooleanLattice.binaryOperator(ValueLattice.getBoolean(el1Common), ValueLattice.getBoolean(el2Common), node.op)
                else if (ValueLattice.elementIsOnlyInteger(el1Common) && ValueLattice.elementIsOnlyInteger(el2Common))
                  IntegerLattice.binaryOperator(ValueLattice.getInteger(el1Common), ValueLattice.getInteger(el2Common), node.op)
                else if (ValueLattice.elementIsOnlyFloat(el1Common) && ValueLattice.elementIsOnlyFloat(el2Common))
                  FloatLattice.binaryOperator(ValueLattice.getFloat(el1Common), ValueLattice.getFloat(el2Common), node.op)
                else if (ValueLattice.elementIsOnlyLong(el1Common) && ValueLattice.elementIsOnlyLong(el2Common))
                  LongLattice.binaryOperator(ValueLattice.getLong(el1Common), ValueLattice.getLong(el2Common), node.op)
                else if (ValueLattice.elementIsOnlyComplex(el1Common) && ValueLattice.elementIsOnlyComplex(el2Common))
                  ComplexLattice.binaryOperator(ValueLattice.getComplex(el1Common), ValueLattice.getComplex(el2Common), node.op)
                else
                  throw new NotImplementedException()
              ValueLattice.leastUpperBound(value, acc)
            } catch {
              case e: ArithmeticException =>
                // Division by zero
                throw new NotImplementedException()
            }
          }
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
    val functionScopeObjectLabel = FunctionScopeObjectLabel(node, node.entry, node.exit)
    val functionFunctionObjectLabel = FunctionObjectLabel(node, node.entry, node.exit, functionScopeObjectLabel)
    val functionObjectLabel = HeapObjectLabel(functionName)
    
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
    writePropertyValueOnVariableObjects(node, functionName, functionObjectValue, result)
  }
  
  def handleClassDeclNode(node: ClassDeclNode, solution: Elt): Elt = {
    solution
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
      } else {
        // Exception: TODO
        AnalysisLattice.setState(solution, node, StateLattice.bottom)
      }
    } else {
      // TODO: Why is heap null?
      AnalysisLattice.setState(solution, node, StateLattice.bottom)
    }
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
        val (newSolution, callGraph) : (Elt,CallGraphLattice.Elt) = objLabels.foldLeft((solution,this.callGraph)) {(acc, objLabel) =>
          val (accSolution, accCallGraph) = acc

          val obj = HeapLattice.getObject(this.heap, objLabel)
          
          val (newSolution, callGraph) : (Elt,CallGraphLattice.Elt) =
            if (objLabel.isInstanceOf[HeapObjectLabel]) {
              handleObjectCall(node, afterCallNode, objLabel.asInstanceOf[HeapObjectLabel], obj, accSolution)
              
            } else if (objLabel.isInstanceOf[FunctionObjectLabel]) {
              handleFunctionObjectCall(node, afterCallNode, objLabel.asInstanceOf[FunctionObjectLabel], obj, accSolution)
              
            } else {
              // Does not occur: elements has been checked to be CallableObjectLabels
              throw new InternalError()
            }
          
          (newSolution,CallGraphLattice.leastUpperBound(callGraph, accCallGraph))
        }
        
        return AnalysisLattice.setCallGraph(newSolution, callGraph)
      }
    } catch {
      case e: NotImplementedException => e.printStackTrace(); AnalysisLattice.setState(solution, node, StateLattice.bottom)
    }
  }
  
  def handleObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, objLabel: HeapObjectLabel, obj: ObjectLattice.Elt, solution: Elt): (Elt,CallGraphLattice.Elt) = {
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
      callObjLabels.foldLeft((solution,CallGraphLattice.bottom)) {(acc, callObjLabel) =>
        val (accSolution, accCallGraph) = acc
        val callObj = HeapLattice.getObject(this.heap, callObjLabel)
        
        if (callObjLabel.isInstanceOf[FunctionObjectLabel]) {
            val (newSolution, callGraph) = handleFunctionObjectCall(callNode, afterCallNode, callObjLabel.asInstanceOf[FunctionObjectLabel], callObj, accSolution)
            (newSolution,CallGraphLattice.leastUpperBound(callGraph, accCallGraph))
          
        } else {
          // TypeError: Trying to call a non-function object
          // Could it be the case that __call__ was set to a non-function object, which has __call__ to a function object?
          throw new NotImplementedException()
        }
      }
    }
  }
  
  def handleFunctionObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, objLabel: FunctionObjectLabel, obj: ObjectLattice.Elt, solution: Elt): (Elt,CallGraphLattice.Elt) = {
    val callGraph = Set[(Any, Node, Any, Node)]((null, callNode, null, objLabel.functionEntryNode), (null, objLabel.functionExitNode, null, afterCallNode))
    var functionScopeObject = HeapLattice.getObject(this.heap, objLabel.scope)

    functionScopeObject = handleFunctionArguments(callNode, functionScopeObject, objLabel.functionEntryNode.funcDef.getInternalArgs())  
    val newSolution = AnalysisLattice.updateHeap(solution, callNode, objLabel.scope, functionScopeObject)

    (newSolution,callGraph)
  }

  //Sets the argument-registers given to the callNode on the functionObjectScope with the correct naming
  def handleFunctionArguments(callNode: CallNode, functionScopeObject: ObjectLattice.Elt, arguments : arguments) : ObjectLattice.Elt = {
    if (callNode.keywordRegs.size > 0) {
      throw new NotImplementedException("Keywords on function calls is not implemented");
    }
    if (callNode.starArgReg != None) {
      throw new NotImplementedException("Star arguments on function calls is not implemented");
    }
    if (callNode.kwArgReg != None) {
      throw new NotImplementedException("kw arguments on function calls is not implemented");
    }

    //Set the parameters as variables in the functionScopeObject
    var args = arguments.getInternalArgs().toList.map(_ match {
      case t: Name => t.getInternalId()
      case _ => throw new NotImplementedException("Other elements than Name was used as arguments in function definition")
    })

    //If the argument size is not equal an exception should potentially be rasied
    if (args.size != callNode.argRegs.size) {
     throw new NotImplementedException("List of registers given as arguments to function does not match the argument length")
    }

    args.zip(callNode.argRegs).foldLeft(functionScopeObject) {(acc,pair) =>
      val (argName,reg) = pair
      val currentArgumentProperty = ObjectLattice.getProperty(functionScopeObject, argName)
      val newArgumentProperty = ObjectPropertyLattice.setValue(ObjectPropertyLattice.bottom, StackFrameLattice.getRegisterValue(this.stackFrame, reg))
      val newMergedProperty = ObjectPropertyLattice.leastUpperBound(currentArgumentProperty, newArgumentProperty)

      ObjectLattice.setProperty(functionScopeObject, argName, newMergedProperty)
    }
  }
  
  def handleAfterCallNode(node: AfterCallNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, -2)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleReturnNode(node: ReturnNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.resultReg)
    val oldValue = StackFrameLattice.getRegisterValue(this.stackFrame, -2)
    AnalysisLattice.updateStackFrame(solution, node, -2, ValueLattice.leastUpperBound(value, oldValue))
  }

  // Global node declares that the variable should be used as a global variable.
  // It is possible to have some value assigned to the variable before this declaration
  // this value should then be moved to the global variable scope, and
  // the property entry for the variable in the current variable object should indicate that
  // it is a global variable.
  def handleGlobalNode(node: GlobalNode, solution: Elt): Elt = {
    // get current abstract value stored for globalnode.variable
    val varObjectLabels = ExecutionContextLattice.getVariableObjects(this.executionContexts)

     val variableValue = varObjectLabels.foldLeft (ValueLattice.bottom) ({(acc, varObjLabel) =>
       val varObj = HeapLattice.getObject(this.heap, varObjLabel)
       val tmpVal = ObjectPropertyLattice.getValue(ObjectLattice.getProperty(varObj, node.variable))
       ValueLattice.leastUpperBound(acc, tmpVal)
     })

    //bind bottom X global for node.varibale in this scope

    //bind variableValue in globalscope
    val getLast = {(l: List[Any]) => l.last}
    val varGlobalObjLabels = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts).map(getLast)

    //module really should be the outer most variable object in all cases
    if (varGlobalObjLabels.size != 1)
      throw new NotImplementedException()

//    val globalVarObject = HeapLattice.getObject(this.heap, varGlobalObjLabels.head)


    solution
  }
  
  def handleClassEntryNode(node: ClassEntryNode, solution: Elt): Elt = {
    solution
  }
}

