package tapy.lattices

import tapy.dfa._

sealed trait NoneElt

object NoneLattice extends Lattice[NoneElt] {
  type Elt = NoneElt

  case class Bottom() extends Elt
  case class None() extends Elt
  
  def top: Elt = None()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (None(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), None()) => false
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == None() || b == None()) None() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else None()
  }
}