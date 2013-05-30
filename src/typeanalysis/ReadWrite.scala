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

trait ReadWrite extends Exceptions with Logger {
  
  /**
    * Variables
    */
  
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
  
  
  /**
    * Properties
    */
  
  var transformedReadPropertyNodes: Set[ReadPropertyNode] = Set()
  
  /**
    * This method is called to transform the CFG in case of a bad attribute access.
    */
  def transformReadPropertyNode(node: ReadPropertyNode): Boolean = {
    if (!this.transformedReadPropertyNodes.contains(node) && node.transform) {
      log("ReadPropertyNode", "Transforming read of attribute '" + node.property + "'")
      
      // Transform the node...
      val readPropertyCfg = CFGNormalizer.getReadPropertyNodeCfg(node, this.worklist.cfg)
      val newCfg = worklist.cfg.replace(node, readPropertyCfg)
      this.worklist.setCFG(newCfg, readPropertyCfg)
      
      this.transformedReadPropertyNodes = this.transformedReadPropertyNodes + node
      
      throw new AttributeError("object has no attribute '" + node.property + "'")
    }
    
    return false
  }
  
  def handleReadPropertyNode(node: ReadPropertyNode, solution: Elt): Elt = {
    
    try {
      val base = node.getRegisterValue(solution, node.baseReg)
      val labels = ValueLattice.getObjectLabels(base)
      
      var res = solution
      
      if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](base)) {
        if (!transformReadPropertyNode(node)) {
          res = Exceptions.raiseNewBuiltInException(node, "AttributeError", res, true)
        }
      }
      
      return labels.foldLeft(res) {(acc, label) =>
        var tmp = acc
        
        val value = node.getPropertyValue(solution, label, node.property)
        if ((value == ValueLattice.bottom || ValueLattice.elementMaybeUndefined(value)) && !transformReadPropertyNode(node)) {
          tmp = Exceptions.raiseNewBuiltInException(node, "AttributeError", acc, true)
        }
        
        node.updateStackFrame(tmp, node.resultReg, value, false)
      }
      
    } catch {
      case e: UnexpectedValueException =>
        log("ReadPropertyNode", e.getMessage())
        AnalysisLattice.setState(solution, node)
      
      case e: AttributeError =>
        // This node will be visited again later by the worklist, so we can just return bottom
        AnalysisLattice.setState(solution, node)
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
  
  
  /**
    * Indexable values
    */
  
  def handleReadIndexableNode(node: ReadIndexableNode, solution: Elt): Elt = {
    solution
  }
  
  def handleWriteIndexableNode(node: WriteIndexableNode, solution: Elt): Elt = {
    solution
  }
}
