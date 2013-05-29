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

object BuiltIn {
  type Elt = AnalysisLattice.Elt

  
  /* Utility functions */
  
  def objectOverwritten(node: Node, solution: Elt): Boolean = {
    Utils.findPropertyValueInScope(node, "object", solution) != objectValue
  }
  
  /**
   * Values
   */
  val noneValue = ValueLattice.setNone(NoneLattice.top)
  val objectLabel = ObjectBuiltinObjectLabel()
  val objectValue = ValueLattice.setObjectLabels(Set(objectLabel))

}