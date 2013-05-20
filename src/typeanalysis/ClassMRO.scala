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

object ClassMRO {
  def Utils = TypeAnalysis
  
  def mro(label: ObjectLabel, state: StateLattice.Elt): Set[List[ObjectLabel]] =
    linearize(label, state)
    
  /**
   * L[C(B1 ... BN)] = C + merge(L[B1] ... L[BN], B1 ... BN)
   */
  def linearize(classLabel: ObjectLabel, state: StateLattice.Elt, indent: String = ""): Set[List[ObjectLabel]] = {
    val (name, bases) = classLabel match {
      case BuiltInClassObjectLabel("object") =>  ("object", List())
      case NewStyleClassObjectLabel(_, entryNode, _, bases) => (entryNode.classDef.getInternalName(), bases)
    }
    
    // Lookup base object labels
    val labels = bases.map{(baseName) =>
      val value = Utils.findPropertyValueInScope(baseName, state, false)
      
      if (value == BuiltIn.objectValue) {
        Set[ObjectLabel](BuiltIn.objectLabel)
        
      } else if (!ValueLattice.elementIsOnlyObjectLabels[NewStyleClassObjectLabel](value)) {
        throw new TypeError("Potentially inheriting from a non-class (" + baseName + ")")
        
      } else {
        ValueLattice.getObjectLabels(value)
      }
    }
    
    val linearizations = labels.foldLeft(Map[ObjectLabel, Set[List[ObjectLabel]]]()) {(acc, labels) =>
      labels.foldLeft(acc) {(acc, label) =>
        if (acc.contains(label)) acc else acc + (label -> linearize(label, state, indent + "    "))
      }
    }
    
    val labelCombinations = combineListOfSets(labels)
    val result = labelCombinations.foldLeft(Set[List[ObjectLabel]]()) {(acc, labelCombination) =>
      prepare(labelCombination, labelCombination, linearizations).foldLeft(acc) {(acc, input) =>
        acc ++ merge(input, state, indent + "    ")
      }
    }
    
    if (result.isEmpty)
      Set(List(classLabel))
    else
      result.map((list) => classLabel :: list)
  }
  
  def prepare(labelCombination: List[ObjectLabel], originalLabelCombination: List[ObjectLabel], linearizationMap: Map[ObjectLabel, Set[List[ObjectLabel]]]): Set[List[List[ObjectLabel]]] = {
    labelCombination match {
      case Nil => Set(List(originalLabelCombination))
      case label :: remaining =>
        val linearizations = linearizationMap.getOrElse(label, Set())
        val remainingLinearizations = prepare(remaining, originalLabelCombination, linearizationMap)
        linearizations.foldLeft(Set[List[List[ObjectLabel]]]()) {(acc, linearization) =>
          acc ++ remainingLinearizations.map((linearizationList) => linearization :: linearizationList)
        }
    }
  }
  /**
   * Merge: take the head of the first list, i.e L[B1][0]; if this
   * head is not in the tail of any of the other lists, then add it
   * to the linearization of C and remove it from the lists in the
   * merge, otherwise look at the head of the next list and take it,
   * if it is a good head. Then repeat the operation until all the 
   * class are removed or it is impossible to find good heads. In this
   * case, it is impossible to construct the merge, Python 2.3 will
   * refuse to create the class C and will raise an exception.
   */
  def merge(labels: List[List[ObjectLabel]], state: StateLattice.Elt, indent: String = ""): Set[List[ObjectLabel]] = {
    if (labels.isEmpty) {
      Set()
      
    } else {
      findGoodHead(labels, labels) match {
        case None => throw new TypeError("Bad MRO")
        case Some(label) =>
          val newLabels = removeFromListOfLists(label, labels)
          val tmp = merge(newLabels, state, indent + "    ")
          if (tmp.isEmpty) Set(List(label)) else tmp.map((list) => label :: list)
      }
    }
  }
  
  def findGoodHead(lists: List[List[ObjectLabel]], originalLists: List[List[ObjectLabel]]): Option[ObjectLabel] = {
    lists match {
      case Nil => None
      case head :: tail =>
        val candidate = head.head
        val good = originalLists.foldLeft(true) {(acc, list) => if (list.tail.contains(candidate)) false else acc }
        if (good)
          Some(candidate)
        else
          findGoodHead(tail, originalLists)
    }
  }
  
  def removeFromList(label: ObjectLabel, list: List[ObjectLabel]): List[ObjectLabel] =
    list.filter((otherLabel) => label != otherLabel)
  
  def removeFromListOfLists(label: ObjectLabel, listOfLists: List[List[ObjectLabel]]): List[List[ObjectLabel]] = {
    listOfLists.foldRight(List[List[ObjectLabel]]()) {(list, acc) =>
      val newList = removeFromList(label, list)
      if (newList.isEmpty) acc else newList :: acc
    }
  }
  
  def combineListOfSets(list: List[Set[ObjectLabel]], indent: String = ""): Set[List[ObjectLabel]] = {
    list match {
      case Nil => Set()
      case set :: remaining =>
        if (remaining == Nil)
          set.map((label) => List(label))
        else {
          val tmp = combineListOfSets(remaining)
          set.foldLeft(Set[List[ObjectLabel]]()) {(acc, setLabel) =>
            tmp.foldLeft(acc) {(acc, tmpList) =>
              acc + (setLabel :: tmpList)
            }
          }
        }
    }
  }
  
  /*
  def ppSetOfListsOfLists(data: Set[List[List[ObjectLabel]]]): String = {
    "{" + data.foldLeft("") {(acc, list) => acc + ppListOfLists(list) + ", " } + "}"
  }
  
  def ppListOfLists(data: List[List[ObjectLabel]]): String = {
    "[" + data.foldLeft("") {(acc, list) => acc + ppList(list) + ", " } + "]"
  }
  
  def ppListOfSetsOfLists(data: List[Set[List[ObjectLabel]]]): String = {
    "[" + data.foldLeft("") {(acc, set) => acc + ppSetOfLists(set) + ", " } + "]"
  }
  
  def ppSetOfLists(data: Set[List[ObjectLabel]]): String = {
    "{" + data.foldLeft("") {(acc, list) => acc + ppList(list) + ", "} + "}" 
  }
  
  def ppList(data: List[ObjectLabel]): String = {
    "[" + data.foldLeft("") {(acc, label) => acc + ppLabel(label) + ", " } + "]"
  }
  
  def ppListOfSets(data: List[Set[ObjectLabel]]): String = {
    "[" + data.foldLeft("") {(acc, set) => acc + ppSet(set) + ", "} + "]" 
  }
  
  def ppSet(data: Set[ObjectLabel]): String = {
    "{" + data.foldLeft("") {(acc, label) => acc + ppLabel(label) + ", " } + "}"
  }
  
  def ppLabel(label: ObjectLabel): String = {
    label match {
      case label: BuiltInClassObjectLabel => label.name
      case label: NewStyleClassObjectLabel => label.entryNode.classDef.getInternalName()
    }
  }
  */
  */
}