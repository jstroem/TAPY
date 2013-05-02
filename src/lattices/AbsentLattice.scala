package tapy.lattices

import tapy.dfa._

object AbsentLattice {
  sealed trait AbsentElt

  case class Bottom() extends AbsentElt
  case class Absent() extends AbsentElt
}

class AbsentLattice extends Lattice[AbsentLattice.AbsentElt] {
  def top: AbsentLattice.AbsentElt = AbsentLattice.Absent()
  def bottom: AbsentLattice.AbsentElt = AbsentLattice.Bottom()
  
  // a >= b
  def compare(a: AbsentLattice.AbsentElt, b: AbsentLattice.AbsentElt): Boolean = return (a, b) match {
    case (AbsentLattice.Absent(), _) => true
    case (AbsentLattice.Bottom(), AbsentLattice.Bottom()) => true
    case (AbsentLattice.Bottom(), AbsentLattice.Absent()) => false
  }
  
  def leastUpperBound(a: AbsentLattice.AbsentElt, b: AbsentLattice.AbsentElt): AbsentLattice.AbsentElt = {
    return if (a == AbsentLattice.Absent() || b == AbsentLattice.Absent()) AbsentLattice.Absent() else AbsentLattice.Bottom()
  }
  
  def greatestLowerBound(a: AbsentLattice.AbsentElt, b: AbsentLattice.AbsentElt): AbsentLattice.AbsentElt = {
    return if (a == AbsentLattice.Bottom() || b == AbsentLattice.Bottom()) AbsentLattice.Bottom() else AbsentLattice.Absent()
  }
}