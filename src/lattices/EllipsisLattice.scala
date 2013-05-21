package tapy.lattices

import tapy.dfa._
import tapy.exceptions._
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.unaryopType


sealed trait EllipsisElt

object EllipsisLattice extends Lattice[EllipsisElt] {
  type Elt = EllipsisElt

  case class Bottom() extends Elt {
    override def toString() = ""
  }
  case class Ellipsis() extends Elt {
    override def toString() = "Ellipsis"
  }
  
  def top: Elt = Ellipsis()
  def bottom: Elt = Bottom()

  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = (e1,e2) match {
    case (Ellipsis(), Ellipsis()) => op match {
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

  def unaryOperator(el: Elt, op: unaryopType): ValueLattice.Elt = el match {
    case (Ellipsis()) => op match {
      case unaryopType.Not => ValueLattice.setBoolean(false)
      case unaryopType.UAdd | unaryopType.USub | unaryopType.Invert => throw new UnaryException("EllipsisType elements cannot do unaryop.Invert", op)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => throw new InternalErrorException("Ellipsis was bottom unaryoperator should not be possible")
  }
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Ellipsis(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), Ellipsis()) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == Ellipsis() || b == Ellipsis()) Ellipsis() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else Ellipsis()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Ellipsis() => s"$indent[Ellipsis (Top)]\n"
    case Bottom() => s"$indent[Not Ellipsis (Bottom)]\n"
    case _ => throw new IllegalArgumentException("EllipsisLattice pattern match error")
  }
}
