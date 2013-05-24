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

trait Exceptions {
  type Elt = AnalysisLattice.Elt
  
  def handleRaiseNode(node: RaiseNode, solution: Elt): Elt = {
    try {
      node.valueReg match {
        case None => throw new NotImplementedException()
        case Some(reg) =>
          val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), reg)
          
          if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](value)) {
            throw new TypeError("Exceptions must be old-style classes or derived from BaseException (actual: " + value + ")")
            
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
            
            node.updateStackFrame(solution, constants.StackConstants.EXCEPTION, value, true)
          }
      }
    } catch {
      case e: TypeError =>
        println("[handleRaiseNode] TypeError: " + e.getMessage())
        node.setState(solution)
    }
  }
}