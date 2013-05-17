package tapy.lattices

import tapy.dfa._
import tapy.exceptions._
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.unaryopType


sealed trait NotImplementedElt

object NotImplementedLattice extends Lattice[NotImplementedElt] {
  type Elt = NotImplementedElt

  case class Bottom() extends Elt {
    override def toString() = "?"
  }
  case class NotImplemented() extends Elt {
    override def toString() = "NotImplemented"
  }
  
  def top: Elt = NotImplemented()
  def bottom: Elt = Bottom()

  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = (e1,e2) match {
    case (NotImplemented(), NotImplemented()) => op match {
      case cmpopType.Eq => BooleanLattice.Concrete(true)
      case cmpopType.NotEq => BooleanLattice.Concrete(false)
      case cmpopType.Lt => BooleanLattice.Concrete(false)
      case cmpopType.LtE => BooleanLattice.Concrete(true)
      case cmpopType.Gt => BooleanLattice.Concrete(false)
      case cmpopType.GtE => BooleanLattice.Concrete(true)
      case _ => BooleanLattice.top
    }
    case _ => BooleanLattice.top
  }

  def unaryOperator(el: Elt, op: unaryopType) : ValueLattice.Elt = el match {
    case (NotImplemented()) => op match {
      case unaryopType.Not => ValueLattice.setBoolean(ValueLattice.bottom, false)
      case unaryopType.UAdd | unaryopType.USub | unaryopType.Invert => throw new UnaryException("NotImplementedType elements cannot do unaryop.Invert", op)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => throw new InternalErrorException("NotImplemented was bottom unaryoperator should not be possible")
  }
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (NotImplemented(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), NotImplemented()) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == NotImplemented() || b == NotImplemented()) NotImplemented() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else NotImplemented()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case NotImplemented() => s"$indent[NotImplemented (Top)]\n"
    case Bottom() => s"$indent[Not NotImplemented (Bottom)]\n"
    case _ => throw new IllegalArgumentException("NotImplementedLattice pattern match error")
  }
}
