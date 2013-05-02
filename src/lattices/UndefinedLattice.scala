package tapy.lattices

import tapy.mfw._

object UndefinedLattice {
  sealed trait UndefinedElt

  case class Bottom() extends UndefinedElt
  case class Undefined() extends UndefinedElt
}

class UndefinedLattice extends Lattice[UndefinedLattice.UndefinedElt] {
  def top: UndefinedLattice.UndefinedElt = UndefinedLattice.Undefined()
  def bottom: UndefinedLattice.UndefinedElt = UndefinedLattice.Bottom()
  
  // a >= b
  def compare(a: UndefinedLattice.UndefinedElt, b: UndefinedLattice.UndefinedElt): Boolean = return (a, b) match {
    case (UndefinedLattice.Undefined(), _) => true
    case (UndefinedLattice.Bottom(), UndefinedLattice.Bottom()) => true
    case (UndefinedLattice.Bottom(), UndefinedLattice.Undefined()) => false
  }
  
  def leastUpperBound(a: UndefinedLattice.UndefinedElt, b: UndefinedLattice.UndefinedElt): UndefinedLattice.UndefinedElt = {
    return if (a == NoneLattice.None() || b == NoneLattice.None()) UndefinedLattice.Undefined() else UndefinedLattice.Bottom()
  }
  
  def greatestLowerBound(a: UndefinedLattice.UndefinedElt, b: UndefinedLattice.UndefinedElt): UndefinedLattice.UndefinedElt = {
    return if (a == NoneLattice.Bottom() || b == NoneLattice.Bottom()) UndefinedLattice.Bottom() else UndefinedLattice.Undefined()
  }
}