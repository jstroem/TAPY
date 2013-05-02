package tapy.lattices

import tapy.dfa._

object UndefinedLattice {
  sealed trait Elt

  case class Bottom() extends Elt
  case class Undefined() extends Elt
}

class UndefinedLattice extends Lattice[UndefinedLattice.Elt] {
  def top: UndefinedLattice.Elt = UndefinedLattice.Undefined()
  def bottom: UndefinedLattice.Elt = UndefinedLattice.Bottom()
  
  // a >= b
  def compare(a: UndefinedLattice.Elt, b: UndefinedLattice.Elt): Boolean = return (a, b) match {
    case (UndefinedLattice.Undefined(), _) => true
    case (UndefinedLattice.Bottom(), UndefinedLattice.Bottom()) => true
    case (UndefinedLattice.Bottom(), UndefinedLattice.Undefined()) => false
  }
  
  def leastUpperBound(a: UndefinedLattice.Elt, b: UndefinedLattice.Elt): UndefinedLattice.Elt = {
    return if (a == UndefinedLattice.Undefined() || b == UndefinedLattice.Undefined()) UndefinedLattice.Undefined() else UndefinedLattice.Bottom()
  }
  
  def greatestLowerBound(a: UndefinedLattice.Elt, b: UndefinedLattice.Elt): UndefinedLattice.Elt = {
    return if (a == UndefinedLattice.Bottom() || b == UndefinedLattice.Bottom()) UndefinedLattice.Bottom() else UndefinedLattice.Undefined()
  }
}