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
      node match {
        case node: ReadPropertyNode =>
          acc.removeNodeAndEdges(node)
             .insert(insertGetAttribute(node), acc.getPredecessors(node), acc.getSuccessors(node))
             
        case _ =>
          acc
      }
    }
  }
  
  def insertGetAttribute(node: ReadPropertyNode): ControlFlowGraph = {
    val hasAttrNode = new HasAttributeNode(node.baseReg, "__getattribute__", CFGGeneratorVisitor.nextRegister())
    val ifNode = new IfNode(hasAttrNode.resultReg)
    val thenNode1 = new ReadPropertyNode(node.baseReg, "__getattribute__", CFGGeneratorVisitor.nextRegister())
    val thenNode2 = new ConstantStringNode(CFGGeneratorVisitor.nextRegister(), node.property)
    val thenNode3 = new CallNode(node.resultReg, thenNode1.resultReg, List(node.baseReg, thenNode2.resultReg))
    val elseNode = node
    
    return new ControlFlowGraph(hasAttrNode)
      .append(ifNode)
      .append(Set(new ControlFlowGraph(thenNode1).append(thenNode2).append(thenNode3),
                  new ControlFlowGraph(elseNode)))
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