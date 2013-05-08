package tapy.lattices

import tapy.dfa._

sealed trait UndefinedElt

object UndefinedLattice extends Lattice[UndefinedElt] {
  type Elt = UndefinedElt
  
  case class Bottom() extends Elt
  case class Undefined() extends Elt
  
  def top: Elt = Undefined()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Undefined(), _) => true
    case (Bottom(), Bottom()) => true
    case (Bottom(), Undefined()) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = {
    return if (a == Undefined() || b == Undefined()) Undefined() else Bottom()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = {
    return if (a == Bottom() || b == Bottom()) Bottom() else Undefined()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Undefined() => s"$indent[Undefined (Top)]\n"
    case Bottom() => s"$indent[Defined (Bottom)]\n"
    case _ => throw new IllegalArgumentException("UndefinedLattice pattern match error")
  }
}
