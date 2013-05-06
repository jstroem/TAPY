package tapy.lattices

import tapy.dfa._

sealed trait AbsentElt

class AbsentLattice extends Lattice[AbsentElt] {
  type Elt = AbsentElt

  case class Bottom() extends Elt
  case class Absent() extends Elt

  def top: Elt = Absent()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Absent(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), Absent()) => false
    case _ => false
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == Absent() || b == Absent()) Absent() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else Absent()
  }
}