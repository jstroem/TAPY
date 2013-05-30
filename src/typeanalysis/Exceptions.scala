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

trait Exceptions extends Logger with Modules {
  
  def handleRaiseNode(node: RaiseNode, solution: Elt): Elt = {
    try {
      node.valueReg match {
        case None => throw new NotImplementedException()
        case Some(reg) =>
          val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), reg)
          
          if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](value)) {
            throw new TypeError("Exceptions must be old-style classes or derived from BaseException (actual (reg. " + reg + "): " + value + ")")
            
          } else {
            val __builtin__ = node.getObject(solution, ModuleScopeObjectLabel("__builtin__"))
            val baseExceptionValue = ObjectLattice.getPropertyValue(__builtin__, "BaseException")
            val baseExceptionLabels = ValueLattice.getObjectLabels(baseExceptionValue)
            
            ValueLattice.getObjectLabels(value).foldLeft() {(acc, label) =>
              label match {
                case label: NewStyleInstanceObjectLabel =>
                  if (!label.definatelyInheritsFrom(baseExceptionLabels, node, solution))
                    throw new TypeError("Exceptions must be old-style classes or derived from BaseException (actual: " + label + ")")
                
                case label: OldStyleInstanceObjectLabel =>
                  Unit
                
                case label =>
                  throw new TypeError("Exceptions must be old-style classes or derived from BaseException (actual: " + label + ")")
              }
            }
            
            log("RaiseNode", "Raising exception")
            node.updateStackFrame(solution, constants.StackConstants.EXCEPTION, value, true)
          }
      }
    } catch {
      case e: TypeError =>
        log("RaiseNode", "TypeError: " + e.getMessage())
        node.setState(solution)
    }
  }
  
  def handleExceptNode(node: ExceptNode, solution: Elt): Elt = {
    val exceptionValue = node.getRegisterValue(solution, StackConstants.EXCEPTION)
    
    if (exceptionValue == ValueLattice.bottom) {
      // Nothing to catch (i.e. infeasible path)
      log("ExceptNode", "Infeasible path")
      node.setState(solution)
    
    } else {
      val exceptionLabels = ValueLattice.getObjectLabels(exceptionValue)
      
      if (node.types.size == 0) {
        // Catch the exception by clearing the exception register
        log("ExceptNode", "Exception caught in catch all except block")
        node.setRegisterValue(solution, StackConstants.EXCEPTION, ValueLattice.bottom, true)
        
      } else {
        throw new NotImplementedException()
      }
    }
  }
  
  object Exceptions {
    
    /**
      * Throws an UnexpectedValueException if the class has not been loaded yet.
      */
    def getNewBuiltinException(node: Node, exceptionType: String, solution: Elt): (NewStyleInstanceObjectLabel, ObjectLattice.Elt) = {
      val builtinModule = Modules.getBuiltinModuleObject(node, solution)
      val classLabel = ValueLattice.getSingleObjectLabel(ObjectLattice.getPropertyValue(builtinModule, exceptionType))
      
      classLabel match {
        case classLabel: NewStyleClassObjectLabel =>
          (NewStyleInstanceObjectLabel(classLabel, node), ObjectLattice.bottom)
        
        case _ =>
          throw new InternalError()
      }
    }
    
    /**
      * Throws an UnexpectedValueException if the class has not been loaded yet.
      */
    def raiseNewBuiltInException(node: Node, exceptionType: String, solution: Elt, strong: Boolean): Elt = {
      val (label, obj) = getNewBuiltinException(node, exceptionType, solution)
      val tmp = node.updateHeap(solution, label, obj)
      node.setRegisterValue(tmp, StackConstants.EXCEPTION, ValueLattice.setObjectLabels(Set(label)), strong)
    }
  }
}