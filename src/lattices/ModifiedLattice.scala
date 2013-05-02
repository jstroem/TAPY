package tapy.lattices

import tapy.dfa._

object ModifiedLattice {
  sealed trait ModifiedElt

  case class Bottom() extends ModifiedElt
  case class Modified() extends ModifiedElt
}

class ModifiedLattice extends Lattice[ModifiedLattice.ModifiedElt] {
  def top: ModifiedLattice.ModifiedElt = ModifiedLattice.Modified()
  def bottom: ModifiedLattice.ModifiedElt = ModifiedLattice.Bottom()
  
  // a >= b
  def compare(a: ModifiedLattice.ModifiedElt, b: ModifiedLattice.ModifiedElt): Boolean = return (a, b) match {
    case (ModifiedLattice.Modified(), _) => true
    case (ModifiedLattice.Bottom(), ModifiedLattice.Bottom()) => true
    case (ModifiedLattice.Bottom(), ModifiedLattice.Modified()) => false
  }
  
  def leastUpperBound(a: ModifiedLattice.ModifiedElt, b: ModifiedLattice.ModifiedElt): ModifiedLattice.ModifiedElt = {
    return if (a == ModifiedLattice.Modified() || b == ModifiedLattice.Modified()) ModifiedLattice.Modified() else ModifiedLattice.Bottom()
  }
  
  def greatestLowerBound(a: ModifiedLattice.ModifiedElt, b: ModifiedLattice.ModifiedElt): ModifiedLattice.ModifiedElt = {
    return if (a == ModifiedLattice.Bottom() || b == ModifiedLattice.Bottom()) ModifiedLattice.Bottom() else ModifiedLattice.Modified()
  }
}