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

class TypeAnalysis
extends Analysis[AnalysisLattice.Elt]
with ClassFunctionDecls with Calls with Constants with Operators with Modules {
  
  /* Declarations */
  
  override type Elt = AnalysisLattice.Elt
  
  var environments: Map[Node, Set[String]] = null
  
  
  /* Analysis interface */
  
  def generateConstraint(node: Node): Constraint[Elt] = node match {
    case node: ImportNode =>  {(solution) => handleImportNode(node, joinPredecessors(node, solution))}
    case node: ModuleEntryNode => {(solution) => handleModuleEntry(node, joinPredecessors(node, solution))}
    
    case node: ConstantBooleanNode => {(solution) => handleConstantBoolean(node, joinPredecessors(node, solution))}
    case node: ConstantIntNode => {(solution) => handleConstantInt(node, joinPredecessors(node, solution))}
    case node: ConstantFloatNode => {(solution) => handleConstantFloat(node, joinPredecessors(node, solution))}
    case node: ConstantLongNode => {(solution) => handleConstantLong(node, joinPredecessors(node, solution))}
    case node: ConstantComplexNode => {(solution) => handleConstantComplex(node, joinPredecessors(node, solution))}
    case node: ConstantStringNode => {(solution) => handleConstantString(node, joinPredecessors(node, solution))}
    case node: ConstantNoneNode => {(solution) => handleConstantNone(node, joinPredecessors(node, solution))}
    
    case node: ReadVariableNode => {(solution) => handleReadVariableNode(node, joinPredecessors(node, solution))}
    case node: WriteVariableNode => {(solution) => handleWriteVariableNode(node, joinPredecessors(node, solution))}
    
    case node: ReadPropertyNode => {(solution) => handleReadPropertyNode(node, joinPredecessors(node, solution))}
    case node: WritePropertyNode => {(solution) => handleWritePropertyNode(node, joinPredecessors(node, solution))}
    
    case node: CompareOpNode => {(solution) => handleCompareOpNode(node, joinPredecessors(node, solution))}
    case node: BinOpNode => {(solution) => handleBinOpNode(node, joinPredecessors(node, solution))}
    case node: UnOpNode => {(solution) => handleUnOpNode(node, joinPredecessors(node, solution))}
    
    case node: FunctionDeclNode => {(solution) => handleFunctionOrUnboundMethodDeclNode(node, joinPredecessors(node, solution))}
    case node: FunctionEntryNode => {(solution) => handleFunctionEntryNode(node, joinPredecessors(node, solution))}
    case node: ExitNode => {(solution) => handleExitNode(node, joinPredecessors(node, solution))}
    case node: CallNode => {(solution) => handleCallNode(node, joinPredecessors(node, solution))}
    case node: ReturnNode => {(solution) => handleReturnNode(node, joinPredecessors(node, solution))}
    case node: AfterCallNode => {(solution) => handleAfterCallNode(node, joinPredecessors(node, solution))}

    case node: GlobalNode => {(solution) => handleGlobalNode(node, joinPredecessors(node, solution))}
    case node: ClassDeclNode => {(solution) => handleClassDeclNode(node, joinPredecessors(node, solution))}
    case node: ClassEntryNode => {(solution) => handleClassEntryNode(node, joinPredecessors(node, solution))}

    case node => {(solution) => joinPredecessors(node, solution) }
  }
  
  def nodeDependencies(node: Node, solution: Elt): Set[Node] = {
    return worklist.cfg.getSuccessors(node) ++ CallGraphLattice.getSuccessors(AnalysisLattice.getCallGraph(solution), node)
  }
  
  var i = 0
  def pp(node: Node, solution: Elt): Unit = {
    HeapLattice.exportToFile(node.getHeap(solution), "solution-" + (i % 2))
    i = i + 1
  }
  
  /**
   * Note that joinPredecessors does not join the state from __init__-ExitNodes to
   * their AfterCallNodes. This is handled by handleAfterCallNode().
   */
  def joinPredecessors(node: Node, solution: Elt): Elt = {
    val predecessors = worklist.cfg.getPredecessors(node) ++ CallGraphLattice.getPredecessorsExceptConstructorReturn(AnalysisLattice.getCallGraph(solution), node)
    
    var state = predecessors.foldLeft(StateLattice.bottom)((acc, pred) =>
      StateLattice.leastUpperBound(acc, pred.getState(solution)))
    
    AnalysisLattice.setState(solution, node, state)
  }
  
  /* Variables */
  
  def handleReadVariableNode(node: ReadVariableNode, solution: Elt): Elt = {
    try {
      val value = Utils.findPropertyValueInScope(node, node.variable, solution, true)
      if (value != ValueLattice.bottom)
        node.updateStackFrame(solution, node.resultReg, value)
      else
        node.variable match {
          case "__BooleanLattice_Concrete_TRUE__" => node.updateStackFrame(solution, node.resultReg, ValueLattice.setBoolean(true))
          case "__BooleanLattice_Concrete_FALSE__" => node.updateStackFrame(solution, node.resultReg, ValueLattice.setBoolean(false))
          case name =>
            throw new NameError("Name '" + name + "' is not defined.")
        }
    } catch {
      case e: NameError => node.setState(solution, StateLattice.bottom)
    }
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.valueReg)
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
  
  //TODO, can we use strong updates??
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
}