package tapy.lattices

import tapy.cfg._
import java.util.UUID
import org.python.antlr.ast.cmpopType
import tapy.lattices._
import tapy.dfa._

abstract class ObjectLabel(id: UUID)

case class ClassObjectLabel(function: ClassEntryNode, id: UUID = UUID.randomUUID()) extends ObjectLabel(id)
case class FunctionObjectLabel(function: FunctionEntryNode, id: UUID = UUID.randomUUID()) extends ObjectLabel(id)
case class ObjectObjectLabel(label: String, id: UUID = UUID.randomUUID()) extends ObjectLabel(id)

object ObjectLabelLattice extends PowerSubSetLattice[ObjectLabel] {
  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = op match {
    case cmpopType.Eq => if (eltSize(e1) == 1 && eltSize(e2) == 1) BooleanLattice.Concrete(e1 == e2) else BooleanLattice.top
    case cmpopType.NotEq => if (eltSize(e1) == 1 && eltSize(e2) == 1) BooleanLattice.Concrete(e1 != e2) else BooleanLattice.top
    case _ => BooleanLattice.top
  }
}
