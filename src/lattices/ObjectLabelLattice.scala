package tapy.lattices

import tapy.cfg._
import tapy.typeanalysis._
import java.util.UUID
import org.python.antlr.ast.cmpopType
import tapy.lattices._
import tapy.dfa._
import tapy.exceptions._

abstract class ObjectLabel()

abstract class CallableObjectLabel() extends ObjectLabel()
abstract class ClassObjectLabel() extends ObjectLabel()

case class ModuleScopeObjectLabel(label: String) extends ObjectLabel() {
  override def toString() = s"Scope $label"
}
case class NewStyleClassObjectLabel(declNode: ClassDeclNode, entryNode: ClassEntryNode, exitNode: ExitNode, bases: List[Set[ObjectLabel]]) extends ClassObjectLabel() {
  override def toString() = s"New Style Class ${entryNode.classDef.getInternalName()}"
  
  def definatelyInheritsFrom(labels: Set[ObjectLabel], node: Node, solution: AnalysisLattice.Elt): Boolean =
    ObjectLabelLattice.definatelyInheritsFrom(this, labels, node, solution)
}
case class OldStyleClassObjectLabel(declNode: ClassDeclNode, entryNode: ClassEntryNode, exitNode: ExitNode, bases: List[Set[ObjectLabel]]) extends ClassObjectLabel() {
  override def toString() = s"Old Style Class ${entryNode.classDef.getInternalName()}"
  
  def definatelyInheritsFrom(labels: Set[ObjectLabel], node: Node, solution: AnalysisLattice.Elt): Boolean =
    ObjectLabelLattice.definatelyInheritsFrom(this, labels, node, solution)
}
case class WrapperObjectLabel(label: FunctionObjectLabel) extends CallableObjectLabel() {
  override def toString() = s"Function Wrapper ${label.entryNode.funcDef.getInternalName()}"
}
case class UnboundMethodObjectLabel(functionLabel: FunctionObjectLabel) extends CallableObjectLabel() {
  override def toString() = s"Unbound Method ${functionLabel.entryNode.funcDef.getInternalName()}"
}
case class BoundMethodObjectLabel(instance: ObjectLabel, functionLabel: FunctionObjectLabel) extends CallableObjectLabel() {
  override def toString() = s"Bound Method ${functionLabel.entryNode.funcDef.getInternalName()}"
}
case class FunctionScopeObjectLabel(declNode: FunctionDeclNode, entryNode: FunctionEntryNode, exitNode: ExitNode) extends ObjectLabel() {
  override def toString() = s"Scope ${entryNode.funcDef.getInternalName()}"
}
case class FunctionObjectLabel(declNode: FunctionDeclNode, entryNode: FunctionEntryNode, exitNode: ExitNode, scopeLabel: FunctionScopeObjectLabel) extends CallableObjectLabel() {
	override def toString() = s"Function ${entryNode.funcDef.getInternalName()}"
}
case class NewStyleInstanceObjectLabel(classLabel: NewStyleClassObjectLabel, allocationSite: CallNode) extends CallableObjectLabel() {
  override def toString() = s"New Style Instance ${classLabel.entryNode.classDef.getInternalName()}"
  
  def definatelyInheritsFrom(labels: Set[ObjectLabel], node: Node, solution: AnalysisLattice.Elt): Boolean =
    ObjectLabelLattice.definatelyInheritsFrom(this, labels, node, solution)
}
case class OldStyleInstanceObjectLabel(classLabel: OldStyleClassObjectLabel, allocationSite: CallNode) extends CallableObjectLabel() {
  override def toString() = s"Old Style Instance ${classLabel.entryNode.classDef.getInternalName()}"
  
  def definatelyInheritsFrom(labels: Set[ObjectLabel], node: Node, solution: AnalysisLattice.Elt): Boolean =
    ObjectLabelLattice.definatelyInheritsFrom(this, labels, node, solution)
}
case class BuiltInClassObjectLabel(name: String) extends ObjectLabel() {
  override def toString() = s"Built In Class $name"
}
case class BuiltInFunctionObjectLabel(name: String) extends ObjectLabel() {
  override def toString() = s"Built In Function $name"
}

object ObjectLabelLattice extends PowerSubSetLattice[ObjectLabel] {
  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = op match {
    case cmpopType.Eq => if (e1.size == 1 && e2.size == 1) BooleanLattice.Concrete(e1 == e2) else BooleanLattice.top
    case cmpopType.NotEq => if (e1.size == 1 && e2.size == 1) BooleanLattice.Concrete(e1 != e2) else BooleanLattice.top
    case _ => BooleanLattice.top
  }

  def definatelyInheritsFrom(subject: ObjectLabel, labels: Set[ObjectLabel], node: Node, solution: AnalysisLattice.Elt): Boolean =
    labels.foldLeft(true) {(acc, label) => definatelyInheritsFrom(subject, label, node, solution) && acc }
  
  def definatelyInheritsFrom(subjectLabel: ObjectLabel, label: ObjectLabel, node: Node, solution: AnalysisLattice.Elt): Boolean = {
    val bases = subjectLabel match {
      case subject: NewStyleClassObjectLabel => subject.bases
      case subject: OldStyleClassObjectLabel => subject.bases
      case subject: NewStyleInstanceObjectLabel => subject.classLabel.bases
      case subject: OldStyleInstanceObjectLabel => subject.classLabel.bases
    }
    
    bases.foldLeft(false) {(acc, baseLabels) =>
      if (acc)
        true
      else {
        if (baseLabels.size == 0)
          false
        else
          // All the possible base labels must inherit from subject, in order to conclude
          // that subject definately inherits from label
          baseLabels.foldLeft(true) {(acc, base) =>
            if (base == label)
              acc
            else
              base match {
                case base: NewStyleClassObjectLabel => acc && definatelyInheritsFrom(base, label, node, solution)
                case base: OldStyleClassObjectLabel => acc && definatelyInheritsFrom(base, label, node, solution)
                case base: BuiltInClassObjectLabel => false
                case _ => throw new TypeError()
              }
          }
      }
    }
  }
  
  def toString(el: Elt) : String =
    if (el == null)
      "(<TOP>)"
    else
      "(" + el.foldLeft("")((acc,objectLabel) => if (acc == "") objectLabel.toString() else acc + ", "+ objectLabel.toString()) + ")"
}