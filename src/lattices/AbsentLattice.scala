package tapy.lattices

import tapy.dfa._

object AbsentLattice {
  sealed trait Elt

  case class Bottom() extends Elt
  case class Absent() extends Elt
}

class AbsentLattice extends Lattice[AbsentLattice.Elt] {
  def top: AbsentLattice.Elt = AbsentLattice.Absent()
  def bottom: AbsentLattice.Elt = AbsentLattice.Bottom()
  
  // a >= b
  def compare(a: AbsentLattice.Elt, b: AbsentLattice.Elt): Boolean = return (a, b) match {
    case (AbsentLattice.Absent(), _) => true
    case (AbsentLattice.Bottom(), AbsentLattice.Bottom()) => true
    case (AbsentLattice.Bottom(), AbsentLattice.Absent()) => false
  }
  
  def leastUpperBound(a: AbsentLattice.Elt, b: AbsentLattice.Elt): AbsentLattice.Elt = {
    return if (a == AbsentLattice.Absent() || b == AbsentLattice.Absent()) AbsentLattice.Absent() else AbsentLattice.Bottom()
  }
  
  def greatestLowerBound(a: AbsentLattice.Elt, b: AbsentLattice.Elt): AbsentLattice.Elt = {
    return if (a == AbsentLattice.Bottom() || b == AbsentLattice.Bottom()) AbsentLattice.Bottom() else AbsentLattice.Absent()
  }
}