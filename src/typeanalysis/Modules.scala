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
  var worklist: Worklist[AnalysisLattice.Elt]
  
  var loadedModules: Set[String] = Set()
  
  type Elt = AnalysisLattice.Elt
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    val moduleLabel = ModuleScopeObjectLabel(node.name)
    
    val tmp = node.name match {
      case "__builtin__" =>
        val moduleObject = ObjectLattice.updatePropertyValues(
          Set(("object", BuiltIn.objectValue),
              ("None", BuiltIn.noneValue),
              ("float", BuiltIn.floatFunctionValue)))
              
        node.updateHeap(solution,
          Set((BuiltIn.objectLabel, ObjectLattice.bottom),
              (BuiltIn.floatFunctionLabel, ObjectLattice.bottom),
              (moduleLabel, moduleObject)))
            
      case _ =>
        node.updateHeap(solution, Set((moduleLabel, ObjectLattice.bottom)))
    }
    
    AnalysisLattice.setExecutionContexts(tmp, node, Set((List(), moduleLabel)))
  }
  
  def handleImportNode(node: ImportNode, solution: Elt): Elt = {
    val moduleName = node.names.last
    val moduleQualifiedName = ASTPrettyPrinter.implodeStringList(node.names, ".", false)
    
    if (!loadedModules.contains(moduleQualifiedName)) {
      // Add the module to the CFG
      println("Add cfg")
      loadedModules = loadedModules + moduleQualifiedName
      
      val moduleCfg = worklist.getCFG(ASTPrettyPrinter.implodeStringList(node.names, "\\", false))
      val newCfg = worklist.cfg.insert(moduleCfg, Set[Node](), worklist.cfg.entryNodes).exportToFile("TEST", true, true)
      worklist.setCFG(newCfg, moduleCfg)
    }
    
    val tmp = Utils.writePropertyValueOnVariableObjects(node, moduleName, ValueLattice.setObjectLabels(Set(ModuleScopeObjectLabel(moduleQualifiedName))), solution, true)
    
    if (node.isImplicit) {
      // Copy variables from imported module
      val moduleScopeObject = node.getObject(solution, ModuleScopeObjectLabel(moduleQualifiedName))
      
      node.getVariableObjects(solution).foldLeft(tmp) {(acc, variableObjectLabel) =>
        val variableObject = node.getObject(solution, variableObjectLabel)
        node.updateHeap(acc, variableObjectLabel, Utils.copyObjectProperties(moduleScopeObject, variableObject))
      }
    } else
      tmp
  }
}