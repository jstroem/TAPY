package tapy.lattices

import tapy.dfa._

sealed trait ModifiedElt

object ModifiedLattice extends Lattice[ModifiedElt] {
  type Elt = ModifiedElt

  case class Bottom() extends Elt
  case class Modified() extends Elt
  
  def top: Elt = Modified()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Modified(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), Modified()) => false
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == Modified() || b == Modified()) Modified() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else Modified()
  }
}