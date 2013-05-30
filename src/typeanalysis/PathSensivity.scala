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

trait PathSensivity extends Logger {
  
  type Elt = AnalysisLattice.Elt
  
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