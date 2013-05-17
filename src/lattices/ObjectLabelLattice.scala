package tapy.lattices

import tapy.cfg._
import java.util.UUID
import org.python.antlr.ast.cmpopType
import tapy.lattices._
import tapy.dfa._

abstract class ObjectLabel()

abstract class CallableObjectLabel() extends ObjectLabel()
abstract class ClassObjectLabel() extends ObjectLabel()

case class ModuleScopeObjectLabel(label: String) extends ObjectLabel() {
  override def toString() = s"Module Scope Object $label"
}
case class NewStyleClassObjectLabel(declNode: ClassDeclNode, entryNode: ClassEntryNode, exitNode: ExitNode, bases: List[String]) extends ClassObjectLabel() {
  override def toString() = s"New Style Class Object ${entryNode.classDef.getInternalName()}"
}
case class OldStyleClassObjectLabel(declNode: ClassDeclNode, entryNode: ClassEntryNode, exitNode: ExitNode, bases: List[String]) extends ClassObjectLabel() {
  override def toString() = s"Old Style Class Object ${entryNode.classDef.getInternalName()}"
}
case class WrapperObjectLabel(label: FunctionObjectLabel) extends CallableObjectLabel() {
  override def toString() = s"Function Wrapper Object ${label.entryNode.funcDef.getInternalName()}"
}
case class UnboundMethodObjectLabel(functionLabel: FunctionObjectLabel) extends CallableObjectLabel() {
  override def toString() = s"Unbound Method Object ${functionLabel.entryNode.funcDef.getInternalName()}"
}
case class BoundMethodObjectLabel(instance: ObjectLabel, functionLabel: FunctionObjectLabel) extends CallableObjectLabel() {
  override def toString() = s"Bound Method Object ${functionLabel.entryNode.funcDef.getInternalName()}"
}
case class FunctionScopeObjectLabel(declNode: FunctionDeclNode, entryNode: FunctionEntryNode, exitNode: ExitNode) extends ObjectLabel() {
  override def toString() = s"Function/Method Scope Object ${entryNode.toString()}"
}
case class FunctionObjectLabel(declNode: FunctionDeclNode, entryNode: FunctionEntryNode, exitNode: ExitNode, scopeLabel: FunctionScopeObjectLabel) extends CallableObjectLabel() {
	override def toString() = s"Function Object ${entryNode.funcDef.getInternalName()}"
}
case class NewStyleInstanceObjectLabel(allocationSite: CallNode) extends CallableObjectLabel() {
  override def toString() = s"New Style Instance Object"
}
case class OldStyleInstanceObjectLabel(allocationSite: CallNode) extends CallableObjectLabel() {
  override def toString() = s"New Style Instance Object"
}
case class BuiltInClassObjectLabel(name: String) extends ObjectLabel() {
  override def toString() = s"Built In Class Object $name"
}

object ObjectLabelLattice extends PowerSubSetLattice[ObjectLabel] {
  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = op match {
    case cmpopType.Eq => if (e1.size == 1 && e2.size == 1) BooleanLattice.Concrete(e1 == e2) else BooleanLattice.top
    case cmpopType.NotEq => if (e1.size == 1 && e2.size == 1) BooleanLattice.Concrete(e1 != e2) else BooleanLattice.top
    case _ => BooleanLattice.top
  }

  def toString(el: Elt) : String =
    if (el == null)
      "(<TOP>)"
    else
      "(" + el.foldLeft("")((acc,objectLabel) => if (acc == "") objectLabel.toString() else acc + ", "+ objectLabel.toString()) + ")"
}