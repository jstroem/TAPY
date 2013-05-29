package tapy.typeanalysis

import tapy.constants.StackConstants
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

class TypeAnalysis(cfg: ControlFlowGraph)
extends Analysis[AnalysisLattice.Elt]
with ClassFunctionDecls with Calls with Constants with Operators with Modules with Environment with Exceptions with Logger {
  
  override type Elt = AnalysisLattice.Elt
  
  override var environment = Environment.build(cfg)
  
  /* Analysis interface */
  
  def generateConstraint(node: Node): Constraint[Elt] = node match {
    case node: ImportNode =>  {(solution) => constraintWrapper(node, solution, ((solution) => handleImportNode(node, solution)))}
    case node: ModuleEntryNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleModuleEntry(node, solution)))}
    
    // Constants
    case node: ConstantBooleanNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantBoolean(node, solution)))}
    case node: ConstantIntNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantInt(node, solution)))}
    case node: ConstantFloatNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantFloat(node, solution)))}
    case node: ConstantLongNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantLong(node, solution)))}
    case node: ConstantComplexNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantComplex(node, solution)))}
    case node: ConstantStringNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantString(node, solution)))}
    case node: ConstantNoneNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleConstantNone(node, solution)))}
    
    case node: ReadVariableNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleReadVariableNode(node, solution)))}
    case node: WriteVariableNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleWriteVariableNode(node, solution)))}
    
    case node: ReadPropertyNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleReadPropertyNode(node, solution)))}
    case node: WritePropertyNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleWritePropertyNode(node, solution)))}

    case node: ReadIndexableNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleReadIndexableNode(node, solution)))}
    case node: WriteIndexableNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleWriteIndexableNode(node, solution)))}
    
    // Operators
    case node: CompareOpNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleCompareOpNode(node, solution)))}
    case node: BinOpNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleBinOpNode(node, solution)))}
    case node: UnOpNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleUnOpNode(node, solution)))}
    
    // ClassFunctionDecls
    case node: ClassDeclNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleClassDeclNode(node, solution)))}
    case node: ClassEntryNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleClassEntryNode(node, solution)))}
    case node: FunctionDeclNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleFunctionOrUnboundMethodDeclNode(node, solution)))}
    case node: FunctionEntryNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleFunctionEntryNode(node, solution)))}
    case node: ExitNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleExitNode(node, solution)))}
    case node: ExceptionalExitNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleExceptionalExitNode(node, solution)))}
    
    // Calls
    case node: CallNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleCallNode(node, solution)))}
    case node: ReturnNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleReturnNode(node, solution)))}
    case node: AfterCallNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleAfterCallNode(node, solution)))}

    // Exceptions
    case node: RaiseNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleRaiseNode(node, solution)))}
    case node: ExceptNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleExceptNode(node, solution)))}
    // case node: TryExceptElseEntryNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleTryExceptElseEntryNode(node, solution)))}
    
    // Misc
    case node: GlobalNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleGlobalNode(node, solution)))}
    case node: AssertNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleAssertNode(node, solution)))}
    
    case node => {(solution) => constraintWrapper(node, solution, ((solution) => solution)) }
  }
  
  def constraintWrapper(node: Node, solution: Elt, constraint: Elt => Elt): Elt = {
   val newSolution = constraint(join(node, solution))
   /* if (solution != newSolution) {
      println("Solution changed for node: " + node)
      println(AnalysisLattice.diff(solution, newSolution, node))
      println()
    }*/
    newSolution
  }
  
  def nodeDependencies(node: Node, solution: Elt): Set[Node] = {
    return worklist.cfg.getSuccessors(node) ++ worklist.cfg.getExceptionSuccessors(node) ++
        CallGraphLattice.getSuccessors(AnalysisLattice.getCallGraph(solution), node) ++
        CallGraphLattice.getExceptionSuccessors(AnalysisLattice.getCallGraph(solution), node)
  }
  
  /**
   * Note that joinPredecessors does not join the state from __init__-ExitNodes to
   * their AfterCallNodes. This is handled by handleAfterCallNode().
   * 
   * Note that AfterCallNodes have special joins too.
   */
  def join(node: Node, solution: Elt): Elt = {
    val state = node match {
      case ExceptNode(_,_,_) | ExceptionalExitNode(_,_,_) =>
        // Only join from predecessors where an exception was thrown for precision
        val predecessors = worklist.cfg.getExceptionPredecessors(node) ++ CallGraphLattice.getExceptionPredecessors(AnalysisLattice.getCallGraph(solution), node)
        predecessors.foldLeft(StateLattice.bottom)((acc, pred) =>
          if (pred.getRegisterValue(solution, StackConstants.EXCEPTION) == ValueLattice.bottom)
            acc
          else
            StateLattice.leastUpperBound(acc, pred.getState(solution)))
      
      case AfterCallNode(_,_) =>
        val callNodes = worklist.cfg.getPredecessors(node)
        val exitNodes = CallGraphLattice.getPredecessorsExceptConstructorReturn(AnalysisLattice.getCallGraph(solution), node)
        
        val callNodesState = callNodes.foldLeft(StateLattice.bottom) {(acc, callNode) =>
          if (exitNodes.size == 0) {
            // This must be a constructor call, where no __init__ is defined.
            // So we should NOT take the heap from the exit nodes!
            StateLattice.leastUpperBound(acc, callNode.getState(solution))
            
          } else {
            StateLattice.leastUpperBound(acc, StateLattice.setStack(StateLattice.bottom, callNode.getStack(solution)))
          }
        }
        
        val exitNodesState = exitNodes.foldLeft(StateLattice.bottom) {(acc, exitNode) =>
          StateLattice.leastUpperBound(exitNode.getState(solution), acc)
        }
        
        StateLattice.leastUpperBound(callNodesState, exitNodesState)
      
      case _ =>
        val predecessors = worklist.cfg.getPredecessors(node) ++
          CallGraphLattice.getPredecessorsExceptConstructorReturn(AnalysisLattice.getCallGraph(solution), node)
        
        predecessors.foldLeft(StateLattice.bottom)((acc, pred) =>
          StateLattice.leastUpperBound(acc, pred.getState(solution)))
    }
    
    AnalysisLattice.setState(solution, node, state)
  }
  
  /* Variables */
  
  def handleReadVariableNode(node: ReadVariableNode, solution: Elt): Elt = {
    try {
      val lookup = Utils.findPropertyValueInScope(node, node.variable, solution)
      val prop = Utils.findPropertyInScope(node, node.variable, solution)

      if (PropertyLattice.isGlobal(prop)) {
        val getLast = {(l: List[ObjectLabel]) => l.last}
        val varGlobalObjLabels = ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution)).map(getLast)

        if (varGlobalObjLabels.size != 1)
          throw new NotImplementedException("assumption failed handleWriteVariableNode")

        val globalValue = ObjectLattice.getPropertyValue(StateLattice.getHeapObject(node.getState(solution), varGlobalObjLabels.head), node.variable)
        node.updateStackFrame(solution, node.resultReg, globalValue)
      }
      else {
        val value =
          if (lookup != ValueLattice.bottom) {
            log("ReadVariableNode", "Successfully read variable " + node.variable)
            lookup
          } else
            node.variable match {
              case "__BooleanLattice_Concrete_TRUE__" => ValueLattice.setBoolean(true)
              case "__BooleanLattice_Concrete_FALSE__" => ValueLattice.setBoolean(false)
              case "__BooleanLattice_Abstract__" => ValueLattice.setBooleanElt(BooleanLattice.Abstract())
              case "__StringLattice_Abstract__" => ValueLattice.setStringElt(StringLattice.Abstract())
              case "__IntegerLattice_Abstract__" => ValueLattice.setIntegerElt(IntegerLattice.Abstract())
              case "__NotImplementedLattice_Concrete__" => ValueLattice.setNotImplemented(NotImplementedLattice.top)
              case "__EllipsisLattice_Concrete__" => ValueLattice.setEllipsis(EllipsisLattice.top)
              case "__Analysis_Register_EXCEPTION__" => StackFrameLattice.getRegisterValue(node.getStackFrame(solution), constants.StackConstants.EXCEPTION)
              case name =>
                if (name.startsWith("__Analysis_Dump_") && name.endsWith("__"))
                  ValueLattice.bottom
                else
                  throw new NameError("Name '" + name + "' is not defined.")
            }

        node.updateStackFrame(solution, node.resultReg, value)
      }
    }
    catch {
        case e: NameError =>
          log("ReadVariableNode", e.getMessage())
          node.setState(solution, StateLattice.bottom)
    }
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val lookup = Utils.findPropertyInScope(node, node.variable, solution)
    val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.valueReg)

    if (PropertyLattice.isGlobal(lookup)) {
      val getLast = {(l: List[ObjectLabel]) => l.last}
      val varGlobalObjLabels = ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution)).map(getLast)

      if (varGlobalObjLabels.size != 1)
        throw new NotImplementedException("assumption failed handleWriteVariableNode")

      Utils.writePropertyValueOnObjectLabelToHeap(node, node.variable, varGlobalObjLabels.head, value, solution, true)
    }
    else     
      Utils.writePropertyValueOnVariableObjects(node, node.variable, value, solution, true)
  }
  
  /* Properties */
  
  def handleReadPropertyNode(node: ReadPropertyNode, solution: Elt): Elt = {
    try {
      val base = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.baseReg)
      
      if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](base)) {
        throw new NotImplementedException("Trying to access property on a non-object")
        
      } else {
        val value = ValueLattice.getObjectLabels(base).foldLeft(ValueLattice.bottom) {(acc, baseLabel) =>
          val basePropertyValue = node.getPropertyValue(solution, baseLabel, node.property)
          ValueLattice.leastUpperBound(basePropertyValue, acc)
        }
        
        node.updateStackFrame(solution, node.resultReg, value)
      }
    } catch {
      case e: NotImplementedException => AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleWritePropertyNode(node: WritePropertyNode, solution: Elt): Elt = {
    try {
      val base = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.baseReg)
      val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.valueReg)
      
      if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](base)) {
        throw new NotImplementedException("Trying to write a property on something that is not an object.")
        
      } else {
        ValueLattice.getObjectLabels(base).foldLeft(solution) {(acc, baseLabel) =>
          if (baseLabel.isInstanceOf[NewStyleClassObjectLabel] || baseLabel.isInstanceOf[OldStyleClassObjectLabel]) {
            // If value is a function, we must wrap that function in a unbound method...
            // First we write all the non-object values
            val tmp = Utils.writePropertyValueOnObjectLabelToHeap(node, node.property, baseLabel, ValueLattice.setObjectLabels(Set(), value), acc)
            
            // Second we write all the object values
            ValueLattice.getObjectLabels(value).foldLeft(tmp) {(acc, valueLabel) =>
              valueLabel match {
                case valueLabel: FunctionObjectLabel =>
                  val functionValue = ValueLattice.setObjectLabels(Set(valueLabel))
                  
                  val methodLabel = UnboundMethodObjectLabel(valueLabel)
                  val methodValue = ValueLattice.setObjectLabels(Set(methodLabel))
                  val methodObject = ObjectLattice.updatePropertyValue("*function*", functionValue)
                  
                  val tmp = node.updateHeap(acc, methodLabel, methodObject)
                  Utils.writePropertyValueOnObjectLabelToHeap(node, node.property, baseLabel, methodValue, tmp)
                  
                case valueLabel =>
                  throw new NotImplementedException()
              }
            }
            
          } else {
            Utils.writePropertyValueOnObjectLabelToHeap(node, node.property, baseLabel, value, acc)
          }
        }
      }
    } catch {
      case e: NotImplementedException => AnalysisLattice.setState(solution, node) 
    }
  }
  
  /** Indexable values **/
  def handleReadIndexableNode(node: ReadIndexableNode, solution: Elt): Elt = {
    solution
  }
  
  def handleWriteIndexableNode(node: WriteIndexableNode, solution: Elt): Elt = {
    solution
  }

  def handleGlobalNode(node: GlobalNode, solution: Elt): Elt = {
    // ObjectProperty representing a global variable
    val bottomGlobalProperty = PropertyLattice.setGlobal(GlobalLattice.Global())

    // get current abstract value stored for globalnode.variable
    val variableObjectLabels = ExecutionContextLattice.getVariableObjects(node.getExecutionContexts(solution))
    val variableProperty = variableObjectLabels.foldLeft(PropertyLattice.bottom) {(acc, varObjLabel) =>
       val varObj = HeapLattice.getObject(node.getHeap(solution), varObjLabel)
       val tmpVal = ObjectLattice.getProperty(varObj, node.variable)
       PropertyLattice.leastUpperBound(acc, tmpVal)
     }

    if (bottomGlobalProperty != variableProperty) {
      //bind node.varibale to {bottom x Global} in this scope
      val updated_solution = Utils.writePropertyOnVariableObjects(node, node.variable, bottomGlobalProperty, solution)

      //bind variableValue in globalscope
      val getLast = {(l: List[ObjectLabel]) => l.last}
      val varGlobalObjLabels = ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution)).map(getLast)

      //module really should be the outer most variable object in all cases
      if (varGlobalObjLabels.size != 1)
        throw new NotImplementedException("handle global er utilfreds")

      return Utils.writePropertyOnObjectLabelToHeap(node, node.variable, varGlobalObjLabels.head, variableProperty, updated_solution)
    }
    else {  // if the variableProperty is already bottom, the job is done
      return solution
    }
  }
  
  def handleAssertNode(node: AssertNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.reg)
    
    if (ValueLattice.elementIsDefinatelyTruthValue(value, node.negate)) {
      log("AssertNode", "Infeasible path: " + value)
      node.setState(solution)
    } else {
      solution
    }
  }
}
