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
with ClassFunctionDecls with Calls with Constants with Operators with Modules with Environment with Exceptions with Logger with ReadWrite with PathSensivity {
  
  override type Elt = AnalysisLattice.Elt
  
  override var environmentVariables = Environment.buildVariables(cfg)
  override var environmentProperties = Environment.buildProperties(cfg)
  
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
    case node: ClassExitNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleClassExitNode(node, solution)))}
    case node: FunctionDeclNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleFunctionOrUnboundMethodDeclNode(node, solution)))}
    case node: FunctionEntryNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleFunctionEntryNode(node, solution)))}
    case node: FunctionExitNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleFunctionExitNode(node, solution)))}
    case node: ExceptionalExitNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleExceptionalExitNode(node, solution)))}
    
    // Calls
    case node: CallNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleCallNode(node, solution)))}
    case node: ReturnNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleReturnNode(node, solution)))}
    case node: AfterCallNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleAfterCallNode(node, solution)))}

    // Exceptions
    case node: RaiseNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleRaiseNode(node, solution)))}
    case node: ExceptNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleExceptNode(node, solution)))}
    
    // Misc
    case node: GlobalNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleGlobalNode(node, solution)))}
    case node: AssertNode => {(solution) => constraintWrapper(node, solution, ((solution) => handleAssertNode(node, solution)))}
    case node: HasAttributeNode => {(solution => constraintWrapper(node, solution, ((solution) => handleHasAttributeNode(node, solution))))}
    
    case node => {(solution) => constraintWrapper(node, solution, ((solution) => solution)) }
  }
  
  def constraintWrapper(node: Node, solution: Elt, constraint: Elt => Elt): Elt = {
   val newSolution = constraint(join(node, solution))
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
        predecessors.foldLeft(StateLattice.bottom) {(acc, pred) =>
          if (pred.getRegisterValue(solution, StackConstants.EXCEPTION) == ValueLattice.bottom)
            acc
          else StateLattice.leastUpperBound(acc, pred.getState(solution))}
      
      case AfterCallNode(_,_) =>
        val callNodes = worklist.cfg.getPredecessors(node)
        val exitNodes = CallGraphLattice.getPredecessorsExceptConstructorReturn(AnalysisLattice.getCallGraph(solution), node)
        
        val callNodesExecutionContexts = callNodes.foldLeft(ExecutionContextLattice.bottom) {(acc, callNode) =>
          ExecutionContextLattice.leastUpperBound(acc, callNode.getExecutionContexts(solution)) }
        val callNodesState = callNodes.foldLeft(StateLattice.bottom) {(acc, callNode) =>
          // Take the stack and the heap
          StateLattice.leastUpperBound(callNode.getState(solution), acc) }
        val exitNodesState = exitNodes.foldLeft(StateLattice.bottom) {(acc, exitNode) =>
          // Take the stack and the heap
          StateLattice.leastUpperBound(exitNode.getState(solution), acc) }
        
        val tmp = StateLattice.updateStackFrame(StateLattice.leastUpperBound(callNodesState, exitNodesState), StackConstants.EXCEPTION, ValueLattice.bottom, true)
        StateLattice.setExecutionContext(tmp, callNodesExecutionContexts)
        
      case _ =>
        val predecessors = worklist.cfg.getPredecessors(node) ++ CallGraphLattice.getPredecessorsExceptConstructorReturn(AnalysisLattice.getCallGraph(solution), node)
        val tmp = predecessors.foldLeft(StateLattice.bottom)((acc, pred) =>
          StateLattice.leastUpperBound(acc, pred.getState(solution)))
        StateLattice.updateStackFrame(tmp, StackConstants.EXCEPTION, ValueLattice.bottom, true)
    }
    
    AnalysisLattice.setState(solution, node, state)
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
  
  def handleHasAttributeNode(node: HasAttributeNode, solution: Elt): Elt = {
    val value = node.getRegisterValue(solution, node.baseReg)
    val labels = ValueLattice.getObjectLabels(value)
    
    if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](value)) {
      if (labels.size == 0)
        return node.setRegisterValue(solution, node.resultReg, ValueLattice.setBoolean(false), true)
      else
        return node.setRegisterValue(solution, node.resultReg, ValueLattice.setBooleanElt(BooleanLattice.top), true)
    }
    
    val (objectsWithAttribute, objectsWithoutAttribute) = ValueLattice.getObjectLabels(value).foldLeft((0, 0)) {(acc, label) =>
      val obj = node.getObject(solution, label)
      val attribute = ObjectLattice.getPropertyValue(obj, node.property)
      
      if (attribute == ValueLattice.bottom || attribute == ValueLattice.undefined)
        (acc._1, acc._2 + 1)
      else if (ValueLattice.elementMaybeUndefined(attribute) && attribute == ValueLattice.undefined)
        (acc._1 + 1, acc._2 + 1)
      else
        (acc._1 + 1, acc._2)
    }
    
    if (objectsWithAttribute == labels.size && objectsWithoutAttribute == 0)
      return node.setRegisterValue(solution, node.resultReg, ValueLattice.setBoolean(true), true)
    else if (objectsWithAttribute == 0 && objectsWithoutAttribute == labels.size)
      return node.setRegisterValue(solution, node.resultReg, ValueLattice.setBoolean(false), true)
    else
      return node.setRegisterValue(solution, node.resultReg, ValueLattice.setBooleanElt(BooleanLattice.top), true)
  }
}