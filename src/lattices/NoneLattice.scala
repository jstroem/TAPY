package tapy.lattices

import tapy.dfa._

object NoneLattice {
  sealed trait Elt

  case class Bottom() extends Elt
  case class None() extends Elt
}

class NoneLattice extends Lattice[NoneLattice.Elt] {
  def top: NoneLattice.Elt = NoneLattice.None()
  def bottom: NoneLattice.Elt = NoneLattice.Bottom()
  
  // a >= b
  def compare(a: NoneLattice.Elt, b: NoneLattice.Elt): Boolean = return (a, b) match {
    case (NoneLattice.None(), _) => true
    case (NoneLattice.Bottom(), NoneLattice.Bottom()) => true
    case (NoneLattice.Bottom(), NoneLattice.None()) => false
  }
  
  def leastUpperBound(a: NoneLattice.Elt, b: NoneLattice.Elt): NoneLattice.Elt = {
    return if (a == NoneLattice.None() || b == NoneLattice.None()) NoneLattice.None() else NoneLattice.Bottom()
  }
  
  def greatestLowerBound(a: NoneLattice.Elt, b: NoneLattice.Elt): NoneLattice.Elt = {
    return if (a == NoneLattice.Bottom() || b == NoneLattice.Bottom()) NoneLattice.Bottom() else NoneLattice.None()
  }
}