package tapy.lattices

import tapy.dfa._

object ModifiedLattice {
  sealed trait Elt

  case class Bottom() extends Elt
  case class Modified() extends Elt
}

class ModifiedLattice extends Lattice[ModifiedLattice.Elt] {
  def top: ModifiedLattice.Elt = ModifiedLattice.Modified()
  def bottom: ModifiedLattice.Elt = ModifiedLattice.Bottom()
  
  // a >= b
  def compare(a: ModifiedLattice.Elt, b: ModifiedLattice.Elt): Boolean = return (a, b) match {
    case (ModifiedLattice.Modified(), _) => true
    case (ModifiedLattice.Bottom(), ModifiedLattice.Bottom()) => true
    case (ModifiedLattice.Bottom(), ModifiedLattice.Modified()) => false
  }
  
  def leastUpperBound(a: ModifiedLattice.Elt, b: ModifiedLattice.Elt): ModifiedLattice.Elt = {
    return if (a == ModifiedLattice.Modified() || b == ModifiedLattice.Modified()) ModifiedLattice.Modified() else ModifiedLattice.Bottom()
  }
  
  def greatestLowerBound(a: ModifiedLattice.Elt, b: ModifiedLattice.Elt): ModifiedLattice.Elt = {
    return if (a == ModifiedLattice.Bottom() || b == ModifiedLattice.Bottom()) ModifiedLattice.Bottom() else ModifiedLattice.Modified()
  }
}