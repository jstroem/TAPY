package tapy.cfg

import org.python.core._
import scala.collection.immutable.Set
import scala.collection.immutable.List
import org.python.antlr.PythonTree
import org.python.antlr.ast._
import org.python.antlr.ast
import org.python.antlr.base._
import scala.collection.JavaConversions._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import java.io._

object CFGMagicMethodsNormalization {
  def normalize(cfg: ControlFlowGraph): ControlFlowGraph = {
    cfg.nodes.foldLeft(cfg) {(acc, node) =>
      return node match {
        case node: ReadVariableNode =>
          val normalized = insertGetAttribute(node)
          val predecessors = acc.getPredecessors(node)
          
          acc.removeEdges(predecessors, node)
             .insert(normalized, predecessors, Set[Node]())
             
        case _ =>
          acc
      }
    }
  }
  
  def insertGetAttribute(node: ReadVariableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
  
  def insertGetAttr(node: ReadVariableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
  
  def insertSetAttr(node: WriteVariableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
  
  def insertDelAttr(node: DelVariableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
  
  def insertGetItem(node: ReadIndexableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
  
  def insertSetItem(node: WriteIndexableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
  
  def insertDelItem(node: DelIndexableNode): ControlFlowGraph = {
    throw new NotImplementedException()
  }
}