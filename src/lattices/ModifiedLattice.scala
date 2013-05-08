package tapy.lattices

import tapy.dfa._

sealed trait ModifiedElt

object ModifiedLattice extends Lattice[ModifiedElt] {
  type Elt = ModifiedElt

  case class Bottom() extends Elt
  case class Modified() extends Elt
  
  def top: Elt = Modified()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Modified(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), Modified()) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == Modified() || b == Modified()) Modified() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else Modified()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Modified() => s"$indent [Modified (Top)]"
    case Bottom() => s"$indent [Not Modified (Bottom)]"
    case _ => throw new IllegalArgumentException("ModifiedLattice pattern match error")
  }
}
