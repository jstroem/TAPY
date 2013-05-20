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
  
  val debugEntry = true
  val debugIntermediate = true
  val debugResult = true
  
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
    
    println()
    if (debugEntry) println(indent + "linearize " + name)
    if (debugIntermediate) println(indent + "bases: " + bases)
    
    // Lookup base object labels
    val powerLabels: List[Set[ObjectLabel]] = bases.map{(baseName) =>
      val value = Utils.findPropertyValueInScope(baseName, state, false)
      
      if (value == BuiltIn.objectValue) {
        Set[ObjectLabel](BuiltIn.objectLabel)
        
      } else if (!ValueLattice.elementIsOnlyObjectLabels[NewStyleClassObjectLabel](value)) {
        if (debugIntermediate) println(indent + "TypeError " + value)
        throw new TypeError("Potentially inheriting from a non-class (" + baseName + ")")
        
      } else {
        ValueLattice.getObjectLabels(value)
      }
    }
    
    if (debugIntermediate) println(indent + "powerLabels: " + ppListOfSets(powerLabels))
    
    // Compute linearizations L[B1], ..., L[BN]. Since Bi may point to several labels,
    // the result of L[Bi] is a list of sets.
    val powerLinearizations = powerLabels.map{(labels) =>
      labels.foldLeft(Set[List[ObjectLabel]]()) {(acc, label) =>
        acc ++ linearize(label, state, indent + "    ") }
    }
    
    val combinations = combine(powerLinearizations, indent + "    ")
    
    println()
    println(indent + "combinations of powerLinearizations:")
    println(indent + "   " + ppListOfSetsOfLists(powerLinearizations))
    println(indent + "is:")
    println(indent + "   " +  ppSetOfListsOfLists(combinations))
    
    // Compute merge(L[B1] ... L[BN])
    val lst = combine(powerLabels.map((labels) => labels.map((label) => List(label))), indent + "    ").foldLeft(Set[List[ObjectLabel]]()) {(acc, lst) =>
      acc + lst.flatten
    }
    
    val input = combinations.foldLeft(Set[List[List[ObjectLabel]]]()) {(acc, list) =>
      lst.foldLeft(acc) {(acc, appendList) => acc + (list :+ appendList)}
    }
    
    val result = input.foldLeft(Set[List[ObjectLabel]]()) {(acc, linearization) =>
      acc ++ merge(linearization, state, indent + "    ")
    }
    
    if (result.isEmpty) {
      if (debugResult) println(indent + "return: " + ppSetOfLists(Set(List(classLabel))))
      
      Set(List(classLabel))
    } else {
      if (debugResult) println(indent + "return: " + ppSetOfLists(result.map((mro) => classLabel :: mro)))
      
      result.map((mro) => classLabel :: mro)
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
    if (debugEntry) println(indent + "merge: " + ppListOfLists(labels))
    
    if (labels.isEmpty) {
      Set()
      
    } else {
      findGoodHead(labels) match {
        case None => throw new TypeError("Bad MRO")
        case Some(label) =>
          val newLabels = removeFromListOfLists(label, labels)
          val tmp = merge(newLabels, state, indent + "    ")
          if (tmp.isEmpty) Set(List(label)) else tmp.map((list) => label :: list)
      }
    }
  }
  
  def mergeAux(before: List[List[ObjectLabel]], current: List[ObjectLabel], after: List[List[ObjectLabel]], state: StateLattice.Elt, indent: String = ""): Set[List[ObjectLabel]] = {
    if (debugEntry) println(indent + "merge: " + ppListOfLists(before) + " - " + ppList(current) + " - " + ppListOfLists(after))
    
    findGoodHead(current :: after) match {
      case None => throw new TypeError("Bad MRO")
      case Some(label) =>
        val newCurrentTail = removeFromList(current.head, current.tail)
        val newAfter = removeFromListOfLists(current.head, after)
        
        val result =
          if (newCurrentTail.isEmpty) {
            if (newAfter.isEmpty) {
              Set()
              
            } else {
              mergeAux(before, newAfter.head, newAfter.tail, state, indent + "    ")
            }
            
          } else {
            mergeAux(before, newCurrentTail, newAfter, state, indent + "    ")
          }
        
        if (result.isEmpty) {
          println(indent + "result: " + ppSetOfLists(Set(List(current.head))))
          Set(List(current.head))
          
        } else {
          println(indent + "result: " + ppSetOfLists(result.map((list) => current.head :: list)))
          result.map((list) => current.head :: list)
        }
    }
  }
  
  def findGoodHead(lists: List[List[ObjectLabel]]): Option[ObjectLabel] = {
    println("findGoodHead: " + ppListOfLists(lists))
    lists match {
      case Nil => None
      case head :: tail =>
        val candidate = head.head
        val good = tail.foldLeft(true) {(acc, tailList) => if (tailList.contains(candidate)) false else acc }
        if (good)
          Some(candidate)
        else
          findGoodHead(tail)
    }
  }
  
  def removeFromList(label: ObjectLabel, list: List[ObjectLabel]): List[ObjectLabel] = {
    list.filter((otherLabel) => label != otherLabel)
  }
  
  def removeFromListOfLists(label: ObjectLabel, listOfLists: List[List[ObjectLabel]]): List[List[ObjectLabel]] = {
    listOfLists.foldRight(List[List[ObjectLabel]]()) {(list, acc) =>
      val newList = list.filter((otherLabel) => label != otherLabel)
      if (newList.isEmpty) acc else newList :: acc
    }
  }
  
  def combine(powerLinearizations: List[Set[List[ObjectLabel]]], indent: String = ""): Set[List[List[ObjectLabel]]] = {
    if (debugEntry) println(indent + "possibleLinearizations: " + ppListOfSetsOfLists(powerLinearizations))
    
    powerLinearizations match {
      case Nil => Set()
      case linearizations :: remaining =>
        if (debugIntermediate) println(indent + "linearizations: " + linearizations)
        if (debugIntermediate) println(indent + "remaining: " + remaining)
        
        val acc = linearizations.map((linearization) => linearization :: List())
        val subLinearizations = combine(remaining, indent + "    ")
        val result = subLinearizations.foldLeft(acc) {(acc, subLinearization) =>
          acc.map((linearization) => linearization ++ subLinearization)
        }
        
        //val result = linearizations.foldLeft(Set[List[List[ObjectLabel]]]()) {(acc, linearization) =>
          //subLinearizations.map((chain) => linearization :: chain) }
        
        if (debugResult) println(indent + "result: " + ppSetOfListsOfLists(result))
        
        result
    }
  }
  
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
}