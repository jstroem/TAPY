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
  def compare(op: cmpopType, e1: Elt, e2: Elt): Option[Boolean] = op match {
    case cmpopType.Eq => if (e1.size == 1 && e2.size == 1) Some(e1 == e2) else None
    case cmpopType.NotEq => if (e1.size == 1 && e2.size == 1) Some(e1 != e2) else None
    case _ => None
  }
}