package tapy.lattices

import tapy.dfa._

sealed trait GlobalElt

object GlobalLattice extends Lattice[GlobalElt] {
  type Elt = GlobalElt

  case class Bottom() extends Elt
  case class Global() extends Elt
  
  def top: Elt = Global()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Global(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), Global()) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == Global() || b == Global()) Global() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else Global()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Global() => s"$indent[Global (Top)]\n"
    case Bottom() => s"$indent[Not Global (Bottom)]\n"
    case _ => throw new IllegalArgumentException("GlobalLattice pattern match error")
  }
}
