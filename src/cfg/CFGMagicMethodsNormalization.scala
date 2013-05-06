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
  def insertGetAttribute(node: ReadPropertyNode): ControlFlowGraph = {
    val tryHasAttrNode = new HasAttributeNode(node.baseReg, "__getattribute__", CFGGeneratorVisitor.nextRegister())
    val tryIfNode = new IfNode(tryHasAttrNode.resultReg)
    val tryThenNode1 = new ReadPropertyNode(node.baseReg, "__getattribute__", CFGGeneratorVisitor.nextRegister())
    val tryThenNode2 = new ConstantStringNode(CFGGeneratorVisitor.nextRegister(), node.property)
    val tryThenNode3 = new CallNode(node.resultReg, tryThenNode1.resultReg, List(tryThenNode2.resultReg))
    val tryElseNode = node
    
    val exceptNode = new ExceptNode(List("AttributeError"), List("e"))
    val exceptHasAttrNode = new HasAttributeNode(node.baseReg, "__getattr__", CFGGeneratorVisitor.nextRegister())
    val exceptIfNode = new IfNode(exceptHasAttrNode.resultReg)
    val exceptThenNode1 = new ReadPropertyNode(node.baseReg, "__getattr__", CFGGeneratorVisitor.nextRegister())
    val exceptThenNode2 = new ConstantStringNode(CFGGeneratorVisitor.nextRegister(), node.property)
    val exceptThenNode3 = new CallNode(node.resultReg, exceptThenNode1.resultReg, List(exceptThenNode2.resultReg))
    val exceptElseNode = new RaiseNode(-1) // TODO
    
    val result = new ControlFlowGraph(tryHasAttrNode)
      .append(tryIfNode)
      .append(Set(new ControlFlowGraph(tryThenNode1).append(tryThenNode2)
                                                    .append(tryThenNode3),
                  new ControlFlowGraph(tryElseNode)))
      .insert(new ControlFlowGraph(exceptNode).append(exceptHasAttrNode)
                                              .append(exceptIfNode)
                                              .append(Set(new ControlFlowGraph(exceptThenNode1).append(exceptThenNode2)
                                                                                               .append(exceptThenNode3),
                                                          new ControlFlowGraph(exceptElseNode))))
      .addExitNode(exceptThenNode3)
      .connectExcept(Set[Node](tryThenNode1, tryThenNode2, tryThenNode3, tryElseNode), exceptNode)
    
    return result
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