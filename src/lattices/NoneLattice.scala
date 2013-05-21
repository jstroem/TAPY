package tapy.lattices

import tapy.dfa._
import tapy.exceptions._
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.unaryopType


sealed trait NoneElt

object NoneLattice extends Lattice[NoneElt] {
  type Elt = NoneElt

  case class Bottom() extends Elt {
    override def toString() = ""
  }
  case class None() extends Elt {
    override def toString() = "None"
  }
  
  def top: Elt = None()
  def bottom: Elt = Bottom()

  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = (e1,e2) match {
    case (None(), None()) => op match {
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
    case (None()) => op match {
      case unaryopType.Not => ValueLattice.setBoolean(true)
      case unaryopType.UAdd | unaryopType.USub | unaryopType.Invert => throw new UnaryException("NoneType elements cannot do unaryop.Invert", op)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => throw new InternalErrorException("None was bottom unaryoperator should not be possible")
  }
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (None(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), None()) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == None() || b == None()) None() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else None()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case None() => s"$indent[None (Top)]\n"
    case Bottom() => s"$indent[Not None (Bottom)]\n"
    case _ => throw new IllegalArgumentException("NoneLattice pattern match error")
  }
}
