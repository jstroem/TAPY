package tapy.lattices

import tapy.cfg._
import java.util.UUID
import org.python.antlr.ast.cmpopType
import tapy.lattices._
import tapy.dfa._

abstract class ObjectLabel(id: UUID) {
	def getIdString() :String = id.toString()
}

abstract class CallableObjectLabel(id: UUID) extends ObjectLabel(id)

case class ClassObjectLabel(classRef: ClassEntryNode, id: UUID = UUID.randomUUID()) extends ObjectLabel(id) {
	override def toString() = s"Class Object ${classRef.toString()}"
}
case class FunctionObjectLabel(functionEntryNode: FunctionEntryNode, scope: ScopeObjectLabel, id: UUID = UUID.randomUUID()) extends CallableObjectLabel(id) {
	override def toString() = s"Function Object ${functionEntryNode.toString()}"
}
case class ObjectObjectLabel(label: String, id: UUID = UUID.randomUUID()) extends CallableObjectLabel(id) {
	override def toString() = s"Real Object $label"
}
case class ScopeObjectLabel(label: String, id: UUID = UUID.randomUUID()) extends ObjectLabel(id) {
	override def toString() = s"Scope Object $label"
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