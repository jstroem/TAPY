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

object BuiltIn {
  val objectLabel = BuiltInClassObjectLabel("object")
  val objectValue = ObjectPropertyLattice.setValue(ObjectPropertyLattice.bottom, ValueLattice.setObjectLabels(Set(objectLabel)))
  val objectElt = ObjectLattice.bottom
}

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
        
        // case node: ReadVariableNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleReadVariableNode(node, join)}
        case node: WritePropertyNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleWritePropertyNode(node, join)}
        
        case node: CompareOpNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleCompareOpNode(node, join)}
        case node: BinOpNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleBinOpNode(node, join)}
        case node: UnOpNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleUnOpNode(node, join)}
        
        case node: FunctionDeclNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleFunctionOrUnboundMethodDeclNode(node, join)}
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
  
  def findPropertyInScope(node: Node, property: String, solution: Elt): ObjectPropertyLattice.Elt = {
    val chains = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts)
    
    // TODO: What if the property is only found in one of the chains? Should add result to also be undefined...
    chains.foldLeft(ObjectPropertyLattice.bottom) {(acc, chain) =>
      val value = chain.foldLeft(ObjectPropertyLattice.bottom) {(acc, objectLabel) =>
        if (acc != ObjectPropertyLattice.bottom)
          acc
        else
          ObjectLattice.getProperty(AnalysisLattice.getHeapObject(node, objectLabel, solution), property)
      }
      ObjectPropertyLattice.leastUpperBound(value, acc)
    }
  }
  
  def findPropertyValueInScope(node: Node, property: String, solution: Elt): ValueLattice.Elt = {
    ObjectPropertyLattice.getValue(findPropertyInScope(node, property, solution))
  }
  
  def writePropertyValueOnObjectLabelToHeap(node: Node, property: String, objectLabel: ObjectLabel, value: ValueLattice.Elt, solution: Elt): Elt = {
    val oldObject = AnalysisLattice.getHeapObject(node, objectLabel, solution)
    val newObject = writePropertyValueOnObject(oldObject, property, value)
    AnalysisLattice.updateHeap(solution, node, objectLabel, newObject)
  }
  
  def writePropertyOnObjectLabelToHeap(node: Node, property: String, objectLabel: ObjectLabel, objectElt: ObjectPropertyLattice.Elt, solution: Elt): Elt = {
    val oldObject = AnalysisLattice.getHeapObject(node, objectLabel, solution)
    val newObject = writePropertyOnObject(oldObject, property, objectElt)
    AnalysisLattice.updateHeap(solution, node, objectLabel, newObject)
  }
  
  def writePropertyValueOnVariableObjects(node: Node, property: String, value: ValueLattice.Elt, solution: Elt): Elt =
    writePropertyOnVariableObjects(node, property, ObjectPropertyLattice.setValue(ObjectPropertyLattice.bottom, value), solution)
  
  def writePropertyOnVariableObjects(node: Node, property: String, value: ObjectPropertyLattice.Elt, solution: Elt): Elt = {
    val variableObjectLabels = AnalysisLattice.getVariableObjects(solution, node)
    variableObjectLabels.foldLeft(solution) {(acc, variableObjectLabel) =>
      val currentVariableObject = HeapLattice.getObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val newVariableObject = writePropertyOnObject(currentVariableObject, property, value)
      AnalysisLattice.updateHeap(acc, node, variableObjectLabel, newVariableObject)
    }
  }
  
  def writePropertyValueOnObject(obj: ObjectLattice.Elt, property: String, value: ValueLattice.Elt): ObjectLattice.Elt =
    writePropertyOnObject(obj, property, ObjectPropertyLattice.setValue(ObjectPropertyLattice.bottom, value))
  
  def writePropertyOnObject(obj: ObjectLattice.Elt, property: String, value: ObjectPropertyLattice.Elt): ObjectLattice.Elt = {
    val currentPropertyValue = ObjectLattice.getProperty(obj, property)
    val newPropertyValue = ObjectPropertyLattice.leastUpperBound(value, currentPropertyValue)
    ObjectLattice.setProperty(obj, property, newPropertyValue)
  }
  
  /* Built in objects */
  
  def builtInObjectOverwritten(node: Node, solution: Elt): Boolean = {
    findPropertyInScope(node, "object", solution) != BuiltIn.objectValue
  }
  
  /* Misc */
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    // Create the main module
    val moduleObjectLabel = ModuleScopeObjectLabel("__main__")
    val moduleObject = ObjectLattice.setProperty(ObjectLattice.bottom, "object", BuiltIn.objectValue)
    
    var result = AnalysisLattice.updateHeap(solution, node, BuiltIn.objectLabel, BuiltIn.objectElt)
    result = AnalysisLattice.updateHeap(result, node, moduleObjectLabel, moduleObject)
    
    AnalysisLattice.setExecutionContexts(result, node, Set((List(), moduleObjectLabel)))
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
    val value = findPropertyValueInScope(node, node.variable, solution)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.valueReg)
    writePropertyValueOnVariableObjects(node, node.variable, value, solution)
  }
  
  /* Properties */
  
  def handleWritePropertyNode(node: WritePropertyNode, solution: Elt): Elt = {
    val base = StackFrameLattice.getRegisterValue(this.stackFrame, node.baseReg)
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.valueReg)
    
    if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](base)) {
      throw new NotImplementedException("Trying to write a property on something that is not an object.")
      
    } else {
      ValueLattice.getObjectLabels(base).foldLeft(solution) {(acc, label) =>
        label match {
          case label: NewStyleClassObjectLabel => println("Newstyle class"); solution
          case label: OldStyleClassObjectLabel => println("Newstyle class"); solution
          case _ =>
            throw new NotImplementedException("Write property nody ")
        }
      }
    }
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
  
  def handleFunctionOrUnboundMethodDeclNode(node: FunctionDeclNode, solution: Elt): Elt = {
    val variableObjectLabels = AnalysisLattice.getVariableObjects(solution, node)
    variableObjectLabels.foldLeft(solution) {(acc, variableObjectLabel) =>
      if (variableObjectLabel.isInstanceOf[NewStyleClassObjectLabel] || variableObjectLabel.isInstanceOf[OldStyleClassObjectLabel]) {
        handleUnboundMethodDeclNode(node, variableObjectLabel, acc)
      } else {
        handleFunctionDeclNode(node, variableObjectLabel, acc)
      }
    }
  }
  
  def handleFunctionDeclNode(node: FunctionDeclNode, variableObjectLabel: ObjectLabel, solution: Elt): Elt = {
    val name = node.entry.funcDef.getInternalName()
    
    // Create labels
    val scopeLabel = FunctionScopeObjectLabel(node, node.entry, node.exit)
    val functionLabel = FunctionObjectLabel(node, node.entry, node.exit, scopeLabel)
    val wrapperLabel = WrapperObjectLabel(functionLabel)
    
    // Create value lattice elements
    val scopeValue = ValueLattice.setObjectLabels(Set(scopeLabel))
    val functionValue = ValueLattice.setObjectLabels(Set(functionLabel))
    val wrapperValue = ValueLattice.setObjectLabels(Set(wrapperLabel))
    
    // Generate scope-object scope chain
    val scopeChain = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts)
    
    // Create objects
    val scopeObject = ObjectLattice.setScopeChain(scopeChain)
    val functionObject = ObjectLattice.updatePropertyValues(Set(("__call__", wrapperValue), ("*scope*", scopeValue)))
    val wrapperObject = ObjectLattice.updatePropertyValue("__call__", wrapperValue)
    
    // Update the lattice
    var result = AnalysisLattice.updateHeap(solution, node, scopeLabel, scopeObject)
    result = AnalysisLattice.updateHeap(result, node, functionLabel, functionObject)
    result = AnalysisLattice.updateHeap(result, node, wrapperLabel, wrapperObject)

    // Add the function name to the current object variables, such that it can be referenced
    writePropertyValueOnObjectLabelToHeap(node, name, variableObjectLabel, functionValue, result)
  }
  
  def handleUnboundMethodDeclNode(node: FunctionDeclNode, variableObjectLabel: ObjectLabel, solution: Elt): Elt = {
    val name = node.entry.funcDef.getInternalName()
    
    // Create labels
    val scopeLabel = FunctionScopeObjectLabel(node, node.entry, node.exit)
    val functionLabel = FunctionObjectLabel(node, node.entry, node.exit, scopeLabel)
    val wrapperLabel = WrapperObjectLabel(functionLabel)
    val methodLabel = UnboundMethodObjectLabel(functionLabel)
    
    // Create value lattice elements
    val scopeValue = ValueLattice.setObjectLabels(Set(scopeLabel))
    val functionValue = ValueLattice.setObjectLabels(Set(functionLabel))
    val wrapperValue = ValueLattice.setObjectLabels(Set(wrapperLabel))
    val methodValue = ValueLattice.setObjectLabels(Set(methodLabel))
    
    // Generate scope-object scope chain
    val scopeChain = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts)
    
    // Create objects
    val scopeObject = ObjectLattice.setScopeChain(scopeChain)
    val functionObject = ObjectLattice.updatePropertyValues(Set(("__call__", wrapperValue), ("*scope*", scopeValue)))
    val wrapperObject = ObjectLattice.updatePropertyValue("__call__", wrapperValue)
    val methodObject = ObjectLattice.updatePropertyValue("*function*", functionValue)
    
    // Update the lattice
    var result = AnalysisLattice.updateHeap(solution, node, scopeLabel, scopeObject)
    result = AnalysisLattice.updateHeap(result, node, functionLabel, functionObject)
    result = AnalysisLattice.updateHeap(result, node, wrapperLabel, wrapperObject)
    result = AnalysisLattice.updateHeap(result, node, methodLabel, methodObject)

    // Add the function name to the current object variables, such that it can be referenced
    writePropertyValueOnObjectLabelToHeap(node, name, variableObjectLabel, methodValue, result)
  }
  
  def isDefinatelyNewStyleClassObject(node: Node, baseNames: List[String], solution: Elt): Boolean = {
    try {
      if (!builtInObjectOverwritten(node, solution)) {
        val allBasesExtendsObject = baseNames.foldLeft(true) {(acc, baseName) =>
          if (baseName == "object")
            true
            
          else {
            val property = findPropertyInScope(node, baseName, solution)
            val value = ObjectPropertyLattice.getValue(property)
            val labels = ValueLattice.getObjectLabels(value)
            
            if (labels.size > 0 && ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](value)) {
              // This particular baseName can be multiple classes (e.g. class C(x): ..., where x has been defined as:
              // if (...): x = C else: x = D), so we must check that each of these possibilities are definately new style class objects!
              labels.foldLeft(true) {(acc, label) =>
                label match {
                  case label: NewStyleClassObjectLabel => acc
                  case label: OldStyleClassObjectLabel => false
                  case label: BuiltInClassObjectLabel => label == BuiltIn.objectLabel
                  case _ => throw new NotImplementedException("Using something that is not a class as base class.")
                }
              }
              
            } else
              throw new NotImplementedException("Using something that is not a class as base class.")
          }
        }
        
        baseNames.size > 0 && allBasesExtendsObject
        
      } else
        // TODO: Could be improved (but the user should really not overwrite the built in object :-)
        false
        
    } catch {
      case e: NotImplementedException => false 
    }
  }
  
  def isDefinatelyOldStyleClassObject(node: Node, baseNames: List[String], solution: Elt): Boolean = {
    try {
      if (!builtInObjectOverwritten(node, solution)) {
        // Built in object has not been overwritten, so we can more or less just search the names for object
        // Note that a variable x can be a pointer to object.
        val noBasesExtendsObject = baseNames.foldLeft(true) {(acc, baseName) =>
          if (baseName == "object")
            false
          
          else {
            val property = findPropertyInScope(node, baseName, solution)
            val value = ObjectPropertyLattice.getValue(property)
            val labels = ValueLattice.getObjectLabels(value)
            
            if (labels.size > 0 && ValueLattice.elementIsOnlyObjectLabels[ClassObjectLabel](value))
              labels.foldLeft(true) {(acc, label) =>
                label match {
                  case label: NewStyleClassObjectLabel => false
                  case label: OldStyleClassObjectLabel => acc
                  case _ =>
                    // Does not occur: value was checked above
                    throw new InternalError()
                }
              }
            else
              throw new NotImplementedException("Using something that is not a class as base class.")
          }
        }
        
        noBasesExtendsObject
        
      } else
        // TODO: Could be improved (but the user should really not overwrite the built in object :-)
        baseNames.size == 0
    } catch {
      case e: NotImplementedException => false 
    }
  }
  
  def handleClassDeclNode(node: ClassDeclNode, solution: Elt): Elt = {
    val className = node.entry.classDef.getInternalName()
    
    val classObjectScopeChain = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts)
    val classObject = ObjectLattice.setScopeChain(classObjectScopeChain)
    
    // Create labels
    val newStyleClassObjectLabel = NewStyleClassObjectLabel(node, node.entry, node.exit, node.bases)
    val oldStyleClassObjectLabel = OldStyleClassObjectLabel(node, node.entry, node.exit, node.bases)
    
    // Update lattice
    if (isDefinatelyNewStyleClassObject(node, node.bases, solution)) {
      val classObjectValue = ValueLattice.setObjectLabels(Set(newStyleClassObjectLabel))
      
      val result = AnalysisLattice.updateHeap(solution, node, newStyleClassObjectLabel, classObject)
      writePropertyValueOnVariableObjects(node, className, classObjectValue, result)
      
    } else if (isDefinatelyOldStyleClassObject(node, node.bases, solution)) {
      val classObjectValue = ValueLattice.setObjectLabels(Set(oldStyleClassObjectLabel))
      
      val result = AnalysisLattice.updateHeap(solution, node, oldStyleClassObjectLabel, classObject)
      writePropertyValueOnVariableObjects(node, className, classObjectValue, result)
      
    } else {
      val classObjectValue = ValueLattice.setObjectLabels(Set(newStyleClassObjectLabel, oldStyleClassObjectLabel))
      
      var result = AnalysisLattice.updateHeap(solution, node, newStyleClassObjectLabel, classObject)
      result = AnalysisLattice.updateHeap(result, node, oldStyleClassObjectLabel, classObject)
      writePropertyValueOnVariableObjects(node, className, classObjectValue, result)
    }
  }
  
  def handleFunctionEntryNode(node: FunctionEntryNode, solution: Elt): Elt = {
    handleClassOrFunctionEntryNode[FunctionScopeObjectLabel](node, ((objectLabel: FunctionScopeObjectLabel) => objectLabel.entryNode), solution)
  }
  
  def handleClassEntryNode(node: ClassEntryNode, solution: Elt): Elt = {
    val entryNode = ((objectLabel: ClassObjectLabel) => objectLabel match {
      case objectLabel: NewStyleClassObjectLabel => objectLabel.entryNode
      case objectLabel: OldStyleClassObjectLabel => objectLabel.entryNode
      case _ => throw new InternalError()
    })
    handleClassOrFunctionEntryNode[ClassObjectLabel](node, entryNode, solution)
  }
  
  def handleClassOrFunctionEntryNode[T <: ObjectLabel: Manifest](node: Node, entryNode: T => Node, solution: Elt): Elt = {
    if (this.heap != null) {
      val scopeObjectLabel = this.heap.foldLeft(null.asInstanceOf[T]: T) {(acc, entry) =>
        if (acc == null) {
          val (objectLabel, _) = entry
          if (manifest[T].erasure.isInstance(objectLabel)) { // GG :-)
            val scopeObjectLabel: T = objectLabel.asInstanceOf[T]
            if (entryNode(scopeObjectLabel) == node) {
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
        AnalysisLattice.setState(solution, node)
      }
    } else {
      // TODO: Why is heap null?
      AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleExitNode(node: ExitNode, solution: Elt): Elt = {
    node.entryNode match {
      case entryNode: FunctionEntryNode =>
        // Set the execution contexts to bottom: Joining at the callee site will ensure that the
        // execution contexts at the after call node will be set to the execution contexts at the
        // call node.
        AnalysisLattice.setExecutionContexts(solution, node, ExecutionContextLattice.bottom)
        
      case entryNode: ClassEntryNode =>
        // Remove the variable object
        AnalysisLattice.setExecutionContexts(solution, node, ExecutionContextLattice.popVariableObject(this.executionContexts))
        
      case entryNode =>
        throw new InternalError()
    }
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
        
      } else {
        ValueLattice.getObjectLabels(function).foldLeft(solution) {(acc, label) =>
          val obj = HeapLattice.getObject(this.heap, label)
          
          label match {
            case label: NewStyleClassObjectLabel => handleClassObjectCall(node, afterCallNode, label, obj, acc)
            case label: OldStyleClassObjectLabel => handleClassObjectCall(node, afterCallNode, label, obj, acc)
            case label: FunctionObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
            case label: WrapperObjectLabel =>
              label.label match {
                case label: FunctionObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
                // case label: UnboundMethodObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
              }
              
            case _ =>
              throw new NotImplementedException("Trying to call a non-callable object")
          }
        }
      }
      
    } catch {
      case e: NotImplementedException =>
        e.printStackTrace()
        AnalysisLattice.setState(solution, node, StateLattice.bottom)
    }
  }
  
  def handleClassObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, classLabel: ClassObjectLabel, classObj: ObjectLattice.Elt, solution: Elt): Elt = {
    // Construct the object
    val (instanceLabel, entryNode, exitNode) = classLabel match {
      case label: NewStyleClassObjectLabel => (NewStyleInstanceObjectLabel(), label.entryNode, label.exitNode)
      case label: OldStyleClassObjectLabel => (OldStyleInstanceObjectLabel(), label.entryNode, label.exitNode)
      case label => throw new InternalError()
    }
    
    val instanceObj = ObjectLattice.bottom
    val instanceValue = ValueLattice.setObjectLabels(Set(instanceLabel))
    
    val init = ObjectLattice.getPropertyValue(classObj, "__init__")
    if (init == ValueLattice.bottom) {
      // __init__ is not defined
      val result = AnalysisLattice.updateHeap(solution, callNode, instanceLabel, instanceObj)
      AnalysisLattice.updateStackFrame(result, callNode, -2, instanceValue)
      
    } else if (ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](init)) {
      // __init__ is defined
      ValueLattice.getObjectLabels(init).foldLeft(solution) {(acc, initLabel) =>
        val initObj = HeapLattice.getObject(this.heap, initLabel)
        
        initLabel match {
          case initLabel: UnboundMethodObjectLabel =>
            AnalysisLattice.setCallGraph(solution, AnalysisLattice.getCallGraph(solution) + ((null, callNode, null, entryNode)) + ((null, exitNode, null, afterCallNode)))
          
          case _ =>
            throw new NotImplementedException("TypeError: Trying to call a non-function object")
        }
      }
      
    } else {
      throw new NotImplementedException("TypeError: Trying to call non-callable object")
    }
  }
  
  /*
  def handleHeapObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, label: FunctionObjectLabel, obj: ObjectLattice.Elt, solution: Elt): Elt = {
    // Check if this is the object of a function
    val call = ObjectLattice.getProperty(obj, "__call__")
    val callValue = ObjectPropertyLattice.getValue(call)
    
    if (call == ObjectPropertyLattice.bottom) {
      // TypeError: Trying to call a non-function object
      throw new NotImplementedException("TypeError: Trying to call a non-function object")
      
    } else if (!ValueLattice.elementIsOnlyObjectLabels[FunctionWrapperObjectLabel](callValue)) {
      // TypeError: Trying to call a non-function object
      throw new NotImplementedException("TypeError: Trying to call a non-function object")
      
    } else {
      ValueLattice.getObjectLabels(callValue).foldLeft(solution) {(acc, callObjLabel) =>
        val callObj = HeapLattice.getObject(this.heap, callObjLabel)
        
        if (callObjLabel.isInstanceOf[FunctionWrapperObjectLabel]) {
            solution // handleFunctionObjectCall(callNode, afterCallNode, callObjLabel.asInstanceOf[FunctionObjectLabel], callObj, acc)
          
        } else {
          // TypeError: Trying to call a non-function object
          // Could it be the case that __call__ was set to a non-function object, which has __call__ to a function object?
          throw new NotImplementedException("TypeError: Trying to call a non-function object")
        }
      }
    }
  }
  */
  
  def handleFunctionObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, functionLabel: FunctionObjectLabel, solution: Elt): Elt = {
    val callGraph = Set[(Any, Node, Any, Node)]((null, callNode, null, functionLabel.entryNode), (null, functionLabel.exitNode, null, afterCallNode))
    var functionScopeObject = HeapLattice.getObject(this.heap, functionLabel.scopeLabel)

    functionScopeObject = handleFunctionArguments(callNode, functionScopeObject, functionLabel.entryNode.funcDef.getInternalArgs(), functionLabel.declNode.defaultArgRegs)  
    val newSolution = AnalysisLattice.updateHeap(solution, callNode, functionLabel.scopeLabel, functionScopeObject)

    AnalysisLattice.setCallGraph(newSolution, AnalysisLattice.getCallGraph(newSolution) ++ callGraph)
  }

  /* Sets the argument-registers given to the callNode on the functionObjectScope with the correct naming */
  def handleFunctionArguments(callNode: CallNode, functionScopeObject: ObjectLattice.Elt, arguments : arguments, defaultArgRegs: List[Int]) : ObjectLattice.Elt = {
    if (callNode.keywordRegs.size > 0)
      throw new NotImplementedException("Keywords on function calls is not implemented");
    else if (callNode.starArgReg != None)
      throw new NotImplementedException("Star arguments on function calls is not implemented");
    else if (callNode.kwArgReg != None)
      throw new NotImplementedException("kw arguments on function calls is not implemented");

    // Set the parameters as variables in the functionScopeObject
    var args = arguments.getInternalArgs().toList.map(_ match {
      case t: Name => t.getInternalId()
      case _ => throw new NotImplementedException("Other elements than Name was used as arguments in function definition")
    })

    // If the argument size is not equal an exception should potentially be rasied
    if (args.size - defaultArgRegs.size > callNode.argRegs.size) {
     throw new NotImplementedException("List of registers given as arguments to function is smaller than required argument length")
    }

    val argRegPairs = args.zipWithIndex.map({case (arg,idx) => 
      if (callNode.argRegs.size > idx) {
        (arg,callNode.argRegs(idx))
      } else {
        (arg,defaultArgRegs(idx - callNode.argRegs.size))
      }
    })

    argRegPairs.foldLeft(functionScopeObject) {(acc,pair) =>
      val (argName,reg) = pair
      writePropertyValueOnObject(acc, argName, StackFrameLattice.getRegisterValue(this.stackFrame, reg))
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
    // ObjectProperty representing a global variable
    val bottomGlobalProperty = ObjectPropertyLattice.setGlobal(ObjectPropertyLattice.bottom, GlobalLattice.Global())

    // get current abstract value stored for globalnode.variable
    val variableObjectLabels = ExecutionContextLattice.getVariableObjects(this.executionContexts)
    val variableProperty = variableObjectLabels.foldLeft (ObjectPropertyLattice.bottom) ({(acc, varObjLabel) =>
       val varObj = HeapLattice.getObject(this.heap, varObjLabel)
       val tmpVal = ObjectLattice.getProperty(varObj, node.variable)
       ObjectPropertyLattice.leastUpperBound(acc, tmpVal)
     })

    if (bottomGlobalProperty != variableProperty) {
      //bind node.varibale to {bottom x Global} in this scope

      writePropertyOnObject

    // //bind variableValue in globalscope
    // val getLast = {(l: List[ObjectLabel]) => l.last}
    // val varGlobalObjLabels = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts).map(getLast)

    // //module really should be the outer most variable object in all cases
    // if (varGlobalObjLabels.size != 1)
    //   throw new NotImplementedException()

    // val globalVarObject = HeapLattice.getObject(this.heap, varGlobalObjLabels.head)
    // val newGlobalVarObj = ObjectLattice.updatePropertyValue(globalVarObject, node.variable, variableValue)
    // val newHeap = HeapLattice.updateHeap(AnalysisLattice.getHeap(node, solution), varGlobalObjLabels.head, newGlobalVarObj)

    }
    else {  // if the variableProperty is already bottom, the job is done


    }




    solution
  }
}

