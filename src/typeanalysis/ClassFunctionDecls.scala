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

trait ClassFunctionDecls extends Environment with Logger {
  
  type Elt = AnalysisLattice.Elt
  
  /**
    * Function and Class Declarations
    */
  
  def handleFunctionOrUnboundMethodDeclNode(node: FunctionDeclNode, solution: Elt): Elt = {
    val variableObjectLabels = node.getVariableObjects(solution)
    variableObjectLabels.foldLeft(solution) {(acc, variableObjectLabel) =>
      if (variableObjectLabel.isInstanceOf[NewStyleClassObjectLabel] || variableObjectLabel.isInstanceOf[OldStyleClassObjectLabel]) {
        handleUnboundMethodDeclNode(node, variableObjectLabel, acc)
      } else {
        handleFunctionDeclNode(node, variableObjectLabel, acc)
      }
    }
  }
  
  def handleFunctionDeclNode(node: FunctionDeclNode, variableObjectLabel: ObjectLabel, solution: Elt): Elt = {
    val name = node.entry.name
    
    // Create labels
    val scopeLabel = FunctionScopeObjectLabel(node, node.entry, node.exit)
    val functionLabel = FunctionObjectLabel(node, node.entry, node.exit, node.exceptionalExitNode, scopeLabel)
    val wrapperLabel = WrapperObjectLabel(functionLabel)
    
    // Create value lattice elements
    val scopeValue = ValueLattice.setObjectLabels(Set(scopeLabel))
    val functionValue = ValueLattice.setObjectLabels(Set(functionLabel))
    val wrapperValue = ValueLattice.setObjectLabels(Set(wrapperLabel))
    
    // Generate scope-object scope chain
    val scopeChain = ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution))
    log("FunctionDeclNode", "Scope chain of " + node.entry.name + " is " + scopeChain.toString())
    
    // Create objects
    val scopeObject = ObjectLattice.setScopeChain(scopeChain)
    val functionObject = ObjectLattice.updatePropertyValues(Set(("__call__", wrapperValue), ("*scope*", scopeValue)))
    val wrapperObject = ObjectLattice.updatePropertyValue("__call__", wrapperValue)
    
    // Update the lattice
    val result = node.updateHeap(solution, Set((scopeLabel, scopeObject), (functionLabel, functionObject), (wrapperLabel, wrapperObject)))

    // Add the function name to the current object variables, such that it can be referenced
    Utils.writePropertyValueOnObjectLabelToHeap(node, name, variableObjectLabel, functionValue, result, true)
  }
  
  def handleUnboundMethodDeclNode(node: FunctionDeclNode, variableObjectLabel: ObjectLabel, solution: Elt): Elt = {
    val name = node.entry.name
    
    // Create labels
    val scopeLabel = FunctionScopeObjectLabel(node, node.entry, node.exit)
    val functionLabel = FunctionObjectLabel(node, node.entry, node.exit, node.exceptionalExitNode, scopeLabel)
    val wrapperLabel = WrapperObjectLabel(functionLabel)
    val methodLabel = UnboundMethodObjectLabel(functionLabel)
    
    // Create value lattice elements
    val scopeValue = ValueLattice.setObjectLabels(Set(scopeLabel))
    val functionValue = ValueLattice.setObjectLabels(Set(functionLabel))
    val wrapperValue = ValueLattice.setObjectLabels(Set(wrapperLabel))
    val methodValue = ValueLattice.setObjectLabels(Set(methodLabel))
    
    // Generate scope-object scope chain
    val scopeChain = ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution))
    
    // Create objects
    val scopeObject = ObjectLattice.setScopeChain(scopeChain)
    val functionObject = ObjectLattice.updatePropertyValues(Set(("__call__", wrapperValue), ("*scope*", scopeValue)))
    val wrapperObject = ObjectLattice.updatePropertyValue("__call__", wrapperValue)
    val methodObject = ObjectLattice.updatePropertyValue("*function*", functionValue)
    
    // Update the lattice
    val result = node.updateHeap(solution, Set((scopeLabel, scopeObject), (functionLabel, functionObject), (wrapperLabel, wrapperObject), (methodLabel, methodObject)))

    // Add the function name to the current object variables, such that it can be referenced
    Utils.writePropertyValueOnObjectLabelToHeap(node, name, variableObjectLabel, methodValue, result, true)
  }
  
  def isDefinatelyNewStyleClassObject(node: Node, baseNames: List[String], solution: Elt): Boolean = {
    try {
      if (!BuiltIn.objectOverwritten(node, solution)) {
        val allBasesExtendsObject = baseNames.foldLeft(true) {(acc, baseName) =>
          if (baseName == "object")
            true
            
          else {
            val property = Utils.findPropertyInScope(node, baseName, solution)
            val value = PropertyLattice.getValue(property)
            val labels = ValueLattice.getObjectLabels(value)
            
            if (labels.size > 0 && ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](value)) {
              // This particular baseName can be multiple classes (e.g. class C(x): ..., where x has been defined as:
              // if (...): x = C else: x = D), so we must check that each of these possibilities are definately new style class objects!
              labels.foldLeft(true) {(acc, label) =>
                label match {
                  case label: NewStyleClassObjectLabel => acc
                  case label: OldStyleClassObjectLabel => false
                  case BuiltIn.objectLabel => true
                  case _ => throw new NotImplementedException("Using something that is not a class as base class.")
                }
              }
              
            } else
              throw new NotImplementedException("Using something that is not a class as base class.")
          }
        }
        
        baseNames.size > 0 && allBasesExtendsObject
        
      } else
        // TODO: Could be improved (but the user should really not overwrite the built in object :-)
        false
        
    } catch {
      case e: NotImplementedException => false 
    }
  }
  
  def isDefinatelyOldStyleClassObject(node: Node, baseNames: List[String], solution: Elt): Boolean = {
    try {
      if (!BuiltIn.objectOverwritten(node, solution)) {
        // Built in object has not been overwritten, so we can more or less just search the names for object
        // Note that a variable x can be a pointer to object.
        val noBasesExtendsObject = baseNames.foldLeft(true) {(acc, baseName) =>
          if (baseName == "object")
            false
          
          else {
            val property = Utils.findPropertyInScope(node, baseName, solution)
            val value = PropertyLattice.getValue(property)
            val labels = ValueLattice.getObjectLabels(value)
            
            if (labels.size > 0 && ValueLattice.elementIsOnlyObjectLabels[ClassObjectLabel](value))
              labels.foldLeft(true) {(acc, label) =>
                label match {
                  case label: NewStyleClassObjectLabel => false
                  case label: OldStyleClassObjectLabel => acc
                  case BuiltIn.objectLabel => true
                  case _ =>
                    // Does not occur: value was checked above
                    throw new InternalError()
                }
              }
            else
              throw new NotImplementedException("Using something that is not a class as base class.")
          }
        }
        
        noBasesExtendsObject
        
      } else
        // TODO: Could be improved (but the user should really not overwrite the built in object :-)
        baseNames.size == 0
    } catch {
      case e: NotImplementedException => false 
    }
  }
  
  def handleClassDeclNode(node: ClassDeclNode, solution: Elt): Elt = {
    val className = node.entry.classDef.getInternalName()
    
    val classObjectScopeChain = ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution))
    val classObject = this.environmentProperties.foldLeft(ObjectLattice.setScopeChain(classObjectScopeChain)) {(acc, property) =>
      ObjectLattice.updatePropertyValue(property, ValueLattice.undefined, acc)
    }
    
    // Create labels
    val bases = node.bases.map{(baseName) =>
      ValueLattice.getObjectLabels(Utils.findPropertyValueInScope(baseName, node.getState(solution)))
    }
    
    val newStyleClassObjectLabel = NewStyleClassObjectLabel(node, node.entry, node.exit, bases)
    val oldStyleClassObjectLabel = OldStyleClassObjectLabel(node, node.entry, node.exit, bases)
    
    // Update lattice
    if (isDefinatelyNewStyleClassObject(node, node.bases, solution)) {
      log("ClassDeclNode", "Class " + node.entry.classDef.getInternalName() + " is definately a new style class")
      val classObjectValue = ValueLattice.setObjectLabels(Set(newStyleClassObjectLabel))
      
      val result = node.updateHeap(solution, newStyleClassObjectLabel, classObject)
      Utils.writePropertyValueOnVariableObjects(node, className, classObjectValue, result, true)
      
    } else if (isDefinatelyOldStyleClassObject(node, node.bases, solution)) {
      val classObjectValue = ValueLattice.setObjectLabels(Set(oldStyleClassObjectLabel))
      
      val result = node.updateHeap(solution, oldStyleClassObjectLabel, classObject)
      Utils.writePropertyValueOnVariableObjects(node, className, classObjectValue, result, true)
      
    } else {
      val classObjectValue = ValueLattice.setObjectLabels(Set(newStyleClassObjectLabel, oldStyleClassObjectLabel))
      
      val result = node.updateHeap(solution, Set((newStyleClassObjectLabel, classObject), (oldStyleClassObjectLabel, classObject)))
      Utils.writePropertyValueOnVariableObjects(node, className, classObjectValue, result, true)
    }
  }
  
  def handleFunctionEntryNode(node: FunctionEntryNode, solution: Elt): Elt = {
    val (scopeObjectLabel, scopeObject) = handleClassOrFunctionEntryNode[FunctionScopeObjectLabel](node, ((objectLabel: FunctionScopeObjectLabel) => objectLabel.entryNode), solution)
      
    if (scopeObjectLabel != null) {
      // Take the current variable object and append it to the scope chain, and
      // set the variable object to the function scope object, such that
      // local declarations will be written onto that object.
      val scopeChains = ObjectLattice.getScopeChain(scopeObject)
      val executionContexts: Set[(List[ObjectLabel], ObjectLabel)] =
        scopeChains.map((scopeChain) => (scopeChain, scopeObjectLabel))
      val tmp = AnalysisLattice.setExecutionContexts(solution, node, executionContexts)
      
      // println()
      // println("New execution context after FunctionEntryNode: " + executionContexts)
      // println("Old was: " + node.getExecutionContexts(solution))
      // println()
      
      this.environmentVariables.getOrElse(node, Set()).foldLeft(tmp) {(acc, variable) =>
        Utils.writePropertyValueOnObjectLabelToHeap(node, variable, scopeObjectLabel, ValueLattice.undefined, acc)
      }
      
    } else {
      // Exception: TODO
      AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleClassEntryNode(node: ClassEntryNode, solution: Elt): Elt = {
    val entryNode = ((objectLabel: ClassObjectLabel) => {
      objectLabel match {
      case objectLabel: NewStyleClassObjectLabel => objectLabel.entryNode
      case objectLabel: OldStyleClassObjectLabel => objectLabel.entryNode
      case _ => throw new InternalError()
    }})
    val (scopeObjectLabel, scopeObject) = handleClassOrFunctionEntryNode[ClassObjectLabel](node, entryNode, solution)
      
    if (scopeObjectLabel != null) {
      // Take the current variable object and append it to the scope chain, and
      // set the variable object to the function scope object, such that
      // local declarations will be written onto that object.
      val executionContexts: Set[(List[ObjectLabel], ObjectLabel)] =
        ExecutionContextLattice.getVariableObjectsOnScopeChains(node.getExecutionContexts(solution)).map({(scopeChain) =>
          (scopeChain, scopeObjectLabel)})
      val tmp = AnalysisLattice.setExecutionContexts(solution, node, executionContexts)
      
      // println()
      // println("New execution context after ClassEntryNode: " + executionContexts)
      // println("Old was: " + node.getExecutionContexts(solution))
      // println()
      
      this.environmentVariables.getOrElse(node, Set()).foldLeft(tmp) {(acc, variable) =>
        Utils.writePropertyValueOnObjectLabelToHeap(node, variable, scopeObjectLabel, ValueLattice.undefined, acc)
      }
      
    } else {
      // Exception: TODO
      AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleClassOrFunctionEntryNode[T <: ObjectLabel: Manifest](node: Node, entryNode: T => Node, solution: Elt): (ObjectLabel, ObjectLattice.Elt) = {
    val heap = node.getHeap(solution)
    if (heap != null) {
      return heap.foldLeft((null.asInstanceOf[T]: T, null: ObjectLattice.Elt)) {(acc, entry) =>
        if (acc._1 == null && acc._2 == null) {
          val (objectLabel, obj) = entry
          if (manifest[T].erasure.isInstance(objectLabel)) { // GG :-)
            val scopeObjectLabel: T = objectLabel.asInstanceOf[T]
            if (entryNode(scopeObjectLabel) == node) {
              (scopeObjectLabel, obj)
            } else acc
          } else acc
        } else acc
      }
    } else {
      return (null, null)
    }
  }
  
  def handleFunctionExitNode(node: FunctionExitNode, solution: Elt): Elt = {
    log("ExitNode", "Handle exit node")
    
    // Update the execution contexts
    // Note: This is done in the after call node for function exit
    solution
  }
  
  def handleClassExitNode(node: ClassExitNode, solution: Elt): Elt = {
    log("ExitNode", "Handle exit node")

    // println()
    // println("New execution context after ClassExitNode: " + ExecutionContextLattice.popVariableObject(node.getExecutionContexts(solution)))
    // println("Old was: " + node.getExecutionContexts(solution))
    // println()
    
    // Update the execution contexts
    AnalysisLattice.setExecutionContexts(solution, node, ExecutionContextLattice.popVariableObject(node.getExecutionContexts(solution)))
  }
  
  /**
    * Does the exact same thing as handleExitNode at the moment.
    */
  def handleExceptionalExitNode(node: ExceptionalExitNode, solution: Elt): Elt = {
    val exception = node.getRegisterValue(solution, StackConstants.EXCEPTION)
    if (exception == ValueLattice.bottom)
      log("ExceptionalExitNode", "Infeasible path")
    else
      log("ExceptionalExitNode", "Uncaught exception: " + exception)
    
    // Update the execution contexts.
    // Note: This is done in the after call node now.
    /*
    AnalysisLattice.setExecutionContexts(solution, node, ExecutionContextLattice.popVariableObject(node.getExecutionContexts(solution)))
    */
    solution
  }
}