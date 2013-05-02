package tapy.lattices

import tapy.mfw._

object NoneLattice {
  sealed trait NoneElt

  case class Bottom() extends NoneElt
  case class None() extends NoneElt
}

class NoneLattice extends Lattice[NoneLattice.NoneElt] {
  def top: NoneLattice.NoneElt = NoneLattice.None()
  def bottom: NoneLattice.NoneElt = NoneLattice.Bottom()
  
  // a >= b
  def compare(a: NoneLattice.NoneElt, b: NoneLattice.NoneElt): Boolean = return (a, b) match {
    case (NoneLattice.None(), _) => true
    case (NoneLattice.Bottom(), NoneLattice.Bottom()) => true
    case (NoneLattice.Bottom(), NoneLattice.None()) => false
  }
  
  def leastUpperBound(a: NoneLattice.NoneElt, b: NoneLattice.NoneElt): NoneLattice.NoneElt = {
    return if (a == NoneLattice.None() || b == NoneLattice.None()) NoneLattice.None() else NoneLattice.Bottom()
  }
  
  def greatestLowerBound(a: NoneLattice.NoneElt, b: NoneLattice.NoneElt): NoneLattice.NoneElt = {
    return if (a == NoneLattice.Bottom() || b == NoneLattice.Bottom()) NoneLattice.Bottom() else NoneLattice.None()
  }
}