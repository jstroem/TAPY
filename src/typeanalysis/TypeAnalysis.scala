package tapy.typeanalysis

import java.lang.ArithmeticException
import org.python.antlr.ast.arguments
import org.python.antlr.ast.Name
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import tapy.exceptions._
import tapy.constants
import scala.collection.JavaConversions._

object BuiltIn {
  val objectLabel = BuiltInClassObjectLabel("object")
  val objectElt = ObjectLattice.bottom

  val objectValue = ValueLattice.setObjectLabels(Set(objectLabel))
  val noneValue = ValueLattice.setNone(NoneLattice.top)
  val falseValue = ValueLattice.setBoolean(false)
  val trueValue = ValueLattice.setBoolean(true)
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
        
        case node: ReadPropertyNode => {(solution) => val join = joinPredecessors(node, solution); init(node, join); handleReadPropertyNode(node, join)}
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
  
  var i = 0
  def pp(node: Node, solution: Elt): Unit = {
    HeapLattice.exportToFile(AnalysisLattice.getHeap(node, solution), "solution-" + (i % 2))
    i = i + 1
  }
  
  /**
   * Note that joinPredecessors does not join the state from __init__-ExitNodes to
   * their AfterCallNodes. This is handled by handleAfterCallNode().
   */
  def joinPredecessors(node: Node, solution: Elt): Elt = {
    val predecessors = cfg.getPredecessors(node) ++ CallGraphLattice.getPredecessorsExceptConstructorReturn(this.callGraph, node)
    
    var state = predecessors.foldLeft(StateLattice.bottom)((acc, pred) =>
      StateLattice.leastUpperBound(acc, AnalysisLattice.getState(pred, solution)))
    
    AnalysisLattice.setState(solution, node, state)
  }

  /**
    * Utility functions
    */
  
  def findPropertyInScope(node: Node, property: String, solution: Elt): PropertyLattice.Elt = {
    val chains = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts)
    
    // TODO: What if the property is only found in one of the chains? Should add result to also be undefined...
    chains.foldLeft(PropertyLattice.bottom) {(acc, chain) =>
      val value = chain.foldLeft(PropertyLattice.bottom) {(acc, objectLabel) =>
        if (acc != PropertyLattice.bottom)
          acc
        else
          ObjectLattice.getProperty(AnalysisLattice.getHeapObject(node, objectLabel, solution), property)
      }
      PropertyLattice.leastUpperBound(value, acc)
    }
  }
  
  def findPropertyValueInScope(node: Node, property: String, solution: Elt): ValueLattice.Elt = {
    PropertyLattice.getValue(findPropertyInScope(node, property, solution))
  }
  
  def writePropertyValueOnObjectLabelToHeap(node: Node, property: String, objectLabel: ObjectLabel, value: ValueLattice.Elt, solution: Elt, strong: Boolean = false): Elt =
    writePropertyOnObjectLabelToHeap(node, property, objectLabel, PropertyLattice.setValue(value), solution, strong)
  
  def writePropertyOnObjectLabelToHeap(node: Node, property: String, objectLabel: ObjectLabel, objectElt: PropertyLattice.Elt, solution: Elt, strong: Boolean = false): Elt = {
    val oldObject = AnalysisLattice.getHeapObject(node, objectLabel, solution)
    val newObject = writePropertyOnObject(oldObject, property, objectElt, strong)
    AnalysisLattice.updateHeap(solution, node, objectLabel, newObject)
  }
  
  def writePropertyValueOnVariableObjects(node: Node, property: String, value: ValueLattice.Elt, solution: Elt, strong: Boolean = false): Elt =
    writePropertyOnVariableObjects(node, property, PropertyLattice.setValue(value), solution, strong)
  
  def writePropertyOnVariableObjects(node: Node, property: String, objectElt: PropertyLattice.Elt, solution: Elt, strong: Boolean = false): Elt = {
    val variableObjectLabels = AnalysisLattice.getVariableObjects(solution, node)
    variableObjectLabels.foldLeft(solution) {(acc, variableObjectLabel) =>
      writePropertyOnObjectLabelToHeap(node, property, variableObjectLabel, objectElt, solution, strong)
    }
  }
  
  def writePropertyValueOnObject(obj: ObjectLattice.Elt, property: String, value: ValueLattice.Elt, strong: Boolean = false): ObjectLattice.Elt =
    writePropertyOnObject(obj, property, PropertyLattice.setValue(value), strong)
  
  def writePropertyOnObject(obj: ObjectLattice.Elt, property: String, value: PropertyLattice.Elt, strong: Boolean = false): ObjectLattice.Elt = {
    val currentPropertyValue = if (strong) PropertyLattice.bottom else ObjectLattice.getProperty(obj, property)
    val newPropertyValue = PropertyLattice.leastUpperBound(value, currentPropertyValue)
    ObjectLattice.setProperty(property, newPropertyValue, obj)
  }
  
  /* Built in objects */
  
  def builtInObjectOverwritten(node: Node, solution: Elt): Boolean = {
    findPropertyValueInScope(node, "object", solution) != BuiltIn.objectValue
  }
  
  /* Misc */
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    // Create the main module
    val moduleObjectLabel = ModuleScopeObjectLabel("__main__")
    var moduleObject = ObjectLattice.updatePropertyValues(Set(("object", BuiltIn.objectValue),
                                                              ("False", BuiltIn.falseValue),
                                                              ("True", BuiltIn.trueValue),
                                                              ("None", BuiltIn.noneValue)))
    
    var result = AnalysisLattice.updateHeap(solution, node, BuiltIn.objectLabel, BuiltIn.objectElt)
    result = AnalysisLattice.updateHeap(result, node, moduleObjectLabel, moduleObject)
    
    AnalysisLattice.setExecutionContexts(result, node, Set((List(), moduleObjectLabel)))
  }

  /*
   * Constants: For each constant we can make a a strong update of the register, because there is
   * a unique register for each register index.
   */
  
  def handleConstantBoolean(node: ConstantBooleanNode, solution: Elt): Elt = {
    val value = ValueLattice.setBoolean(node.bool)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantInt(node: ConstantIntNode, solution: Elt): Elt = {
    val value = ValueLattice.setInteger(node.int.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantFloat(node: ConstantFloatNode, solution: Elt): Elt = {
    val value = ValueLattice.setFloat(node.float.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantLong(node: ConstantLongNode, solution: Elt): Elt = {
    val value = ValueLattice.setLong(node.long.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantComplex(node: ConstantComplexNode, solution: Elt): Elt = {
    val value = ValueLattice.setComplex(node.complex.getReal().getValue(), node.complex.getImag().getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantString(node: ConstantStringNode, solution: Elt): Elt = {
    val value = ValueLattice.setString(node.string)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantNone(node: ConstantNoneNode, solution: Elt): Elt = {
    val value = ValueLattice.setNone(NoneLattice.top)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  /* Variables */
  
  def handleReadVariableNode(node: ReadVariableNode, solution: Elt): Elt = {
    val value = findPropertyValueInScope(node, node.variable, solution)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.valueReg)
    writePropertyValueOnVariableObjects(node, node.variable, value, solution, true)
  }
  
  /* Properties */
  
  def handleReadPropertyNode(node: ReadPropertyNode, solution: Elt): Elt = {
    try {
      val base = StackFrameLattice.getRegisterValue(this.stackFrame, node.baseReg)
      
      if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](base)) {
        throw new NotImplementedException("Trying to access property on a non-object")
        
      } else {
        val value = ValueLattice.getObjectLabels(base).foldLeft(ValueLattice.bottom) {(acc, baseLabel) =>
          val baseObject = AnalysisLattice.getHeapObject(node, baseLabel, solution)
          val basePropertyValue = ObjectLattice.getPropertyValue(baseObject, node.property)
          ValueLattice.leastUpperBound(basePropertyValue, acc)
        }
        
        AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
      }
    } catch {
      case e: NotImplementedException => AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleWritePropertyNode(node: WritePropertyNode, solution: Elt): Elt = {
    try {
      val base = StackFrameLattice.getRegisterValue(this.stackFrame, node.baseReg)
      val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.valueReg)
      
      if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](base)) {
        throw new NotImplementedException("Trying to write a property on something that is not an object.")
        
      } else {
        ValueLattice.getObjectLabels(base).foldLeft(solution) {(acc, baseLabel) =>
          if (baseLabel.isInstanceOf[NewStyleClassObjectLabel] || baseLabel.isInstanceOf[OldStyleClassObjectLabel]) {
            // If value is a function, we must wrap that function in a unbound method...
            // First we write all the non-object values
            val tmp = writePropertyValueOnObjectLabelToHeap(node, node.property, baseLabel, ValueLattice.setObjectLabels(Set(), value), acc)
            
            // Second we write all the object values
            ValueLattice.getObjectLabels(value).foldLeft(tmp) {(acc, valueLabel) =>
              valueLabel match {
                case valueLabel: FunctionObjectLabel =>
                  val functionValue = ValueLattice.setObjectLabels(Set(valueLabel))
                  
                  val methodLabel = UnboundMethodObjectLabel(valueLabel)
                  val methodValue = ValueLattice.setObjectLabels(Set(methodLabel))
                  val methodObject = ObjectLattice.updatePropertyValue("*function*", functionValue)
                  
                  val tmp = AnalysisLattice.updateHeap(acc, node, methodLabel, methodObject)
                  writePropertyValueOnObjectLabelToHeap(node, node.property, baseLabel, methodValue, tmp)
                  
                case valueLabel =>
                  throw new NotImplementedException()
              }
            }
            
          } else {
            writePropertyValueOnObjectLabelToHeap(node, node.property, baseLabel, value, acc)
          }
        }
      }
    } catch {
      case e: NotImplementedException => AnalysisLattice.setState(solution, node) 
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
            val value = PropertyLattice.getValue(property)
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
            val value = PropertyLattice.getValue(property)
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
        AnalysisLattice.setExecutionContexts(solution, node)
        
      case entryNode: ClassEntryNode =>
        // Remove the variable object
        AnalysisLattice.setExecutionContexts(solution, node, ExecutionContextLattice.popVariableObject(this.executionContexts))
        
      case entryNode =>
        throw new InternalError()
    }
  }
  
  def handleCallNode(node: CallNode, solution: Elt): Elt = {
    val afterCallNode = cfg.getSuccessors(node).head.asInstanceOf[AfterCallNode]
    
    // Clear the return registers
    var tmp = AnalysisLattice.updateStackFrame(solution, node, constants.StackConstants.RETURN, ValueLattice.bottom, true)
    tmp = AnalysisLattice.updateStackFrame(tmp, node, constants.StackConstants.RETURN_CONSTRUCTOR, ValueLattice.bottom, true)
    
    try {
      val function: ValueLattice.Elt = StackFrameLattice.getRegisterValue(this.stackFrame, node.functionReg)
      
      if (function == ValueLattice.bottom) {
        // TypeError: Potentially trying to call a non-object
        throw new NotImplementedException()
        
      } else if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](function)) {
        // TypeError: Potentially trying to call a non-object
        throw new NotImplementedException()
        
      } else {
        ValueLattice.getObjectLabels(function).foldLeft(tmp) {(acc, label) =>
          val obj = HeapLattice.getObject(this.heap, label)
          
          label match {
            case label: NewStyleClassObjectLabel => handleClassObjectCall(node, afterCallNode, label, obj, acc)
            case label: OldStyleClassObjectLabel => handleClassObjectCall(node, afterCallNode, label, obj, acc)
            case label: FunctionObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
            case label: BoundMethodObjectLabel => handleBoundMethodObjectCall(node, afterCallNode, label, acc)
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
        AnalysisLattice.setState(tmp, node)
    }
  }
  
  def handleClassObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, classLabel: ClassObjectLabel, classObj: ObjectLattice.Elt, solution: Elt): Elt = {
    // Construct the object
    val instanceLabel = classLabel match {
      case label: NewStyleClassObjectLabel => NewStyleInstanceObjectLabel(label, callNode)
      case label: OldStyleClassObjectLabel => OldStyleInstanceObjectLabel(label, callNode)
      case label => throw new InternalError()
    }
    
    val instanceValue = ValueLattice.setObjectLabels(Set(instanceLabel))
    
    var tmp = AnalysisLattice.updateHeap(solution, callNode, instanceLabel, ObjectLattice.setProperty("__class__", PropertyLattice.setValue(ValueLattice.setObjectLabels(Set(classLabel)))))
    
    tmp = ObjectLattice.getProperties(classObj) match {
      case PropertiesLattice.Top() => throw new NotImplementedException()
      case PropertiesLattice.Concrete(classProperties) =>
        classProperties.foldLeft(tmp) {(acc, entry) =>
          val (property, propertyElt) = entry
          val value = PropertyLattice.getValue(propertyElt)
          
          // First we write all the non-object values.
          // Note! Don't write propertyElt to the acc: a local may be declared to be global in the class,
          // but it will not be global for its instances!
          val tmp = writePropertyValueOnObjectLabelToHeap(callNode, property, instanceLabel, ValueLattice.setObjectLabels(Set(), value), acc)
          
          // Second we write all the object values
          ValueLattice.getObjectLabels(value).foldLeft(tmp) {(acc, valueLabel) =>
            valueLabel match {
              case valueLabel: UnboundMethodObjectLabel =>
                val functionValue = ValueLattice.setObjectLabels(Set(valueLabel))
                
                val methodLabel = BoundMethodObjectLabel(instanceLabel, valueLabel.functionLabel)
                val methodValue = ValueLattice.setObjectLabels(Set(methodLabel))
                val methodObject = ObjectLattice.updatePropertyValue("*function*", functionValue)
                
                val tmp = AnalysisLattice.updateHeap(acc, callNode, methodLabel, methodObject)
                writePropertyValueOnObjectLabelToHeap(callNode, property, instanceLabel, methodValue, tmp)
                
              case valueLabel =>
                throw new NotImplementedException()
            }
          }
        }
      
      case _ =>
        throw new InternalError()
    }
    
    val instanceObj = AnalysisLattice.getHeapObject(callNode, instanceLabel, tmp)
    
    val init = ObjectLattice.getPropertyValue(instanceObj, "__init__")
    if (init == ValueLattice.bottom) {
      // __init__ is not defined
      AnalysisLattice.updateStackFrame(tmp, callNode, constants.StackConstants.RETURN_CONSTRUCTOR, instanceValue)
      
    } else if (ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](init)) {
      // __init__ is defined
      ValueLattice.getObjectLabels(init).foldLeft(tmp) {(acc, initLabel) =>
        initLabel match {
          case initLabel: BoundMethodObjectLabel =>
            tmp = handleFunctionArguments(callNode, initLabel.functionLabel, tmp, Some(ValueLattice.setObjectLabels(Set(initLabel.instance))))
            tmp = AnalysisLattice.updateCallGraph(tmp, Set((null, callNode, null, initLabel.functionLabel.entryNode, false), (null, initLabel.functionLabel.exitNode, null, afterCallNode, false)))
            AnalysisLattice.updateStackFrame(tmp, callNode, constants.StackConstants.RETURN_CONSTRUCTOR, instanceValue)
            
          case initLabel =>
            throw new NotImplementedException("TypeError: Trying to call a non-function object")
        }
      }
      
    } else {
      throw new NotImplementedException("TypeError: Trying to call non-callable object")
    }
  }
  
  def handleFunctionObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, functionLabel: FunctionObjectLabel, solution: Elt): Elt = {
    val tmp = handleFunctionArguments(callNode, functionLabel, solution)
    AnalysisLattice.updateCallGraph(tmp, Set((null, callNode, null, functionLabel.entryNode, true), (null, functionLabel.exitNode, null, afterCallNode, true)))
  }
  
  def handleBoundMethodObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, methodLabel: BoundMethodObjectLabel, solution: Elt): Elt = {
    val tmp = handleFunctionArguments(callNode, methodLabel.functionLabel, solution, Some(ValueLattice.setObjectLabels(Set(methodLabel.instance)))) // TODO
    AnalysisLattice.updateCallGraph(tmp, Set((null, callNode, null, methodLabel.functionLabel.entryNode, true), (null, methodLabel.functionLabel.exitNode, null, afterCallNode, true)))
  }
  
  /* Sets the argument-registers given to the callNode on the functionObjectScope with the correct naming */
  def handleFunctionArguments(callNode: CallNode, functionLabel: FunctionObjectLabel, solution: Elt, receiver: Option[ValueLattice.Elt] = None): Elt = {
    var functionScopeObject = HeapLattice.getObject(this.heap, functionLabel.scopeLabel)
    
    if (callNode.keywordRegs.size > 0)
      throw new NotImplementedException("Keywords on function calls is not implemented");
    else if (callNode.starArgReg != None)
      throw new NotImplementedException("Star arguments on function calls is not implemented");
    else if (callNode.kwArgReg != None)
      throw new NotImplementedException("kw arguments on function calls is not implemented");

    // Set the parameters as variables in the functionScopeObject
    var args = functionLabel.entryNode.funcDef.getInternalArgs().getInternalArgs().toList.map(_ match {
      case t: Name => t.getInternalId()
      case _ => throw new NotImplementedException("Other elements than Name was used as arguments in function definition")
    })

    // If the argument size is not equal an exception should potentially be rasied
    if (args.size - functionLabel.declNode.defaultArgRegs.size > callNode.argRegs.size + receiver.size) {
     throw new NotImplementedException("List of registers given as arguments to function is smaller than required argument length")
    }

    val argsWithoutReceiver = if (receiver.size == 1) args.tail else args // Remove "self"
    val argRegPairs = argsWithoutReceiver.zipWithIndex.map({case (arg,idx) => 
      if (callNode.argRegs.size > idx) {
        (arg, callNode.argRegs(idx))
      } else {
        (arg, functionLabel.declNode.defaultArgRegs(idx - callNode.argRegs.size))
      }
    })

    functionScopeObject = receiver match {
      case Some(receiver) => writePropertyValueOnObject(functionScopeObject, args(0), receiver)
      case None => functionScopeObject
    }
    
    functionScopeObject = argRegPairs.foldLeft(functionScopeObject) {(acc,pair) =>
      val (argName,reg) = pair
      writePropertyValueOnObject(acc, argName, StackFrameLattice.getRegisterValue(this.stackFrame, reg))
    }
    
    AnalysisLattice.updateHeap(solution, callNode, functionLabel.scopeLabel, functionScopeObject)
  }
  
  /**
   * joinPredecessors() does not join constructor call edges, so we do that here!
   * 
   * handleAfterCallNode must remember to clear the return registers:
   * Otherwise, x = A(), where A.__init__ creates an object of type B(), would
   * result in x being either an object of type A or B.
   */
  def handleAfterCallNode(node: AfterCallNode, solution: Elt): Elt = {
    try {
      // Join constructor call edges!
      val state = CallGraphLattice.getConstructorCallPredecessors(this.callGraph, node).foldLeft(StateLattice.bottom) {(acc, pred) =>
        // Check that __init__ returns None
        val initReturnValue = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(pred, solution), constants.StackConstants.RETURN)
        if (!ValueLattice.elementIsOnlyNone(initReturnValue)) {
          throw new TypeError("__init__() should return None (actual: " + initReturnValue + ")")
        }
        
        // Clear the return register (ensures that a=C() => a=C(), and not A=C() or A=None)
        val predState = StateLattice.updateStackFrame(AnalysisLattice.getState(pred, solution), constants.StackConstants.RETURN, ValueLattice.bottom, true)
        StateLattice.leastUpperBound(acc, predState)
      }
      
      var tmp = AnalysisLattice.setState(solution, node, StateLattice.leastUpperBound(AnalysisLattice.getState(node, solution), state))
      
      // Get the returned values and store them
      val value = StackFrameLattice.getRegisterValue(this.stackFrame, constants.StackConstants.RETURN)
      val valueConstructor = StackFrameLattice.getRegisterValue(this.stackFrame, constants.StackConstants.RETURN_CONSTRUCTOR)
      
      tmp = AnalysisLattice.updateStackFrame(tmp, node, node.resultReg, ValueLattice.leastUpperBound(value, valueConstructor))
      
      // Clear the return registers:
      tmp = AnalysisLattice.updateStackFrame(tmp, node, constants.StackConstants.RETURN, ValueLattice.bottom, true)
      AnalysisLattice.updateStackFrame(tmp, node, constants.StackConstants.RETURN_CONSTRUCTOR, ValueLattice.bottom, true)
      
    } catch {
      case e: TypeError => AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleReturnNode(node: ReturnNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(this.stackFrame, node.resultReg)
    val oldValue = StackFrameLattice.getRegisterValue(this.stackFrame, constants.StackConstants.RETURN)
    AnalysisLattice.updateStackFrame(solution, node, constants.StackConstants.RETURN, ValueLattice.leastUpperBound(value, oldValue))
  }

  //TODO, can we use strong updates??
  def handleGlobalNode(node: GlobalNode, solution: Elt): Elt = {
    // ObjectProperty representing a global variable
    val bottomGlobalProperty = PropertyLattice.setGlobal(GlobalLattice.Global())

    // get current abstract value stored for globalnode.variable
    val variableObjectLabels = ExecutionContextLattice.getVariableObjects(this.executionContexts)
    val variableProperty = variableObjectLabels.foldLeft(PropertyLattice.bottom) {(acc, varObjLabel) =>
       val varObj = HeapLattice.getObject(this.heap, varObjLabel)
       val tmpVal = ObjectLattice.getProperty(varObj, node.variable)
       PropertyLattice.leastUpperBound(acc, tmpVal)
     }

    if (bottomGlobalProperty != variableProperty) {
      //bind node.varibale to {bottom x Global} in this scope
      val updated_solution = writePropertyOnVariableObjects(node, node.variable, bottomGlobalProperty, solution)

      //bind variableValue in globalscope
      val getLast = {(l: List[ObjectLabel]) => l.last}
      val varGlobalObjLabels = ExecutionContextLattice.getVariableObjectsOnScopeChains(this.executionContexts).map(getLast)

      //module really should be the outer most variable object in all cases
      if (varGlobalObjLabels.size != 1)
        throw new NotImplementedException("handle global er utilfreds")

      return writePropertyOnObjectLabelToHeap(node, node.variable, varGlobalObjLabels.head, variableProperty, updated_solution)
    }
    else {  // if the variableProperty is already bottom, the job is done
      return solution
    }
  }
}