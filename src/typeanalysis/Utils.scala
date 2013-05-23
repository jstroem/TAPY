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

object Utils {
  type Elt = AnalysisLattice.Elt
  
  def copyObjectProperties(from: ObjectLattice.Elt, to: ObjectLattice.Elt, overwrite: Boolean = false): ObjectLattice.Elt = {
    ObjectLattice.getProperties(from) match {
      case PropertiesLattice.Top() => to
      case PropertiesLattice.Concrete(map) =>
        map.foldLeft(to) {(acc, entry) =>
          val (property, value) = entry
          if (overwrite || ObjectLattice.getProperty(to, property) == PropertyLattice.bottom)
            ObjectLattice.setProperty(property, value, acc)
          else
            acc
        }
      case _ => throw new InternalError()
    }
  }
  
  def findPropertyValueInScope(node: Node, property: String, solution: Elt, print: Boolean = false): ValueLattice.Elt = findPropertyValueInScope(property, node.getState(solution), print)
  def findPropertyValueInScope(property: String, state: StateLattice.Elt, print: Boolean): ValueLattice.Elt =
    PropertyLattice.getValue(findPropertyInScope(property, state, print))
  
  def findPropertyInScope(node: Node, property: String, solution: Elt): PropertyLattice.Elt = findPropertyInScope(property, node.getState(solution))
  def findPropertyInScope(property: String, state: StateLattice.Elt, print: Boolean = false): PropertyLattice.Elt = {
    val chains = ExecutionContextLattice.getVariableObjectsOnScopeChains(StackLattice.getExecutionContext(StateLattice.getStack(state)))
    
    // TODO: What if the property is only found in one of the chains? Should add result to also be undefined...
    chains.foldLeft(PropertyLattice.bottom) {(acc, chain) =>
      val value = chain.foldLeft(PropertyLattice.bottom) {(acc, objectLabel) =>
        if (print)
          try {
            objectLabel match {
              case objectLabel: NewStyleClassObjectLabel =>
                println("--- MRO of " + objectLabel.entryNode.classDef.getInternalName() + "---")
                ClassMRO.linearize(objectLabel, state).foldLeft() {(acc, chain) =>
                  chain.foldLeft() {(acc, label) =>
                    label match {
                      case label: BuiltInClassObjectLabel => println(label.name)
                      case label: NewStyleClassObjectLabel => println(label.entryNode.classDef.getInternalName())
                    }
                  }}
              case _ =>
            }
          } catch { case e: Exception => e.printStackTrace()}
        
        if (acc != PropertyLattice.bottom)
          acc
        else
          ObjectLattice.getProperty(StateLattice.getHeapObject(state, objectLabel), property)
      }
      PropertyLattice.leastUpperBound(value, acc)
    }
  }
  
  def writePropertyValueOnObjectLabelToHeap(node: Node, property: String, objectLabel: ObjectLabel, value: ValueLattice.Elt, solution: Elt, strong: Boolean = false): Elt =
    writePropertyOnObjectLabelToHeap(node, property, objectLabel, PropertyLattice.setValue(value), solution, strong)
  
  def writePropertyOnObjectLabelToHeap(node: Node, property: String, objectLabel: ObjectLabel, objectElt: PropertyLattice.Elt, solution: Elt, strong: Boolean = false): Elt = {
    val oldObject = node.getObject(solution, objectLabel)
    val newObject = writePropertyOnObject(oldObject, property, objectElt, strong)
    node.updateHeap(solution, objectLabel, newObject)
  }
  
  def writePropertyValueOnVariableObjects(node: Node, property: String, value: ValueLattice.Elt, solution: Elt, strong: Boolean = false): Elt =
    writePropertyOnVariableObjects(node, property, PropertyLattice.setValue(value), solution, strong)
  
  def writePropertyOnVariableObjects(node: Node, property: String, objectElt: PropertyLattice.Elt, solution: Elt, strong: Boolean = false): Elt = {
    val variableObjectLabels = node.getVariableObjects(solution)
    variableObjectLabels.foldLeft(solution) {(acc, variableObjectLabel) =>
      writePropertyOnObjectLabelToHeap(node, property, variableObjectLabel, objectElt, solution, strong)
    }
  }
  
  def writePropertyValueOnObject(obj: ObjectLattice.Elt, property: String, value: ValueLattice.Elt, strong: Boolean = false): ObjectLattice.Elt =
    writePropertyOnObject(obj, property, PropertyLattice.setValue(value), strong)
  
  def writePropertyOnObject(obj: ObjectLattice.Elt, property: String, value: PropertyLattice.Elt, strong: Boolean = false): ObjectLattice.Elt = {
    val currentPropertyValue = if (strong) PropertyLattice.bottom else ObjectLattice.getProperty(obj, property)
    val newPropertyValue = PropertyLattice.leastUpperBound(value, currentPropertyValue)
    ObjectLattice.setProperty(property, newPropertyValue, obj)
  }
}