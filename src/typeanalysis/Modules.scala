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

trait Modules {
  type Elt = AnalysisLattice.Elt
  
  def handleImportNode(node: ImportNode, solution: Elt): Elt = {
    solution
  }
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    // Create the main module
    val moduleObjectLabel = ModuleScopeObjectLabel("__main__")
    var moduleObject = ObjectLattice.updatePropertyValues(Set(("object", BuiltIn.objectValue),
                                                              ("False", BuiltIn.falseValue),
                                                              ("True", BuiltIn.trueValue),
                                                              ("None", BuiltIn.noneValue)))
    
    val result = node.updateHeap(solution, Set((BuiltIn.objectLabel, BuiltIn.objectElt), (moduleObjectLabel, moduleObject)))
    AnalysisLattice.setExecutionContexts(result, node, Set((List(), moduleObjectLabel)))
  }
}