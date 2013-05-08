package tapy.lattices

import tapy.dfa._

sealed trait BooleanElt

object BooleanLattice extends Lattice[BooleanElt] {
  type Elt = BooleanElt
  
  case class Concrete(b: Boolean) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(true), Concrete(true)) => true
    case (Concrete(false), Concrete(false)) => true
    case _ => false
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()

    case (Concrete(true), Concrete(true)) => Concrete(true)
    case (Concrete(false), Concrete(false)) => Concrete(false)


    case (Concrete(false), Bottom()) => Concrete(false)
    case (Concrete(true), Bottom()) => Concrete(true)

    case (Bottom(), Concrete(false)) => Concrete(false)
    case (Bottom(), Concrete(true)) => Concrete(true)

    case (Bottom(), Bottom()) => Bottom()

    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()

    case (Concrete(true), Concrete(true)) => Concrete(true)
    case (Concrete(false), Concrete(false)) => Concrete(false)


    case (Concrete(false), Abstract()) => Concrete(false)
    case (Concrete(true), Abstract()) => Concrete(true)

    case (Abstract(), Concrete(false)) => Concrete(false)
    case (Abstract(), Concrete(true)) => Concrete(true)

    case (Abstract(), Abstract()) => Abstract()

    case (_, _) =>  Bottom()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent boolean $l"
    case Bottom() => s"$indent BottomBoolean"
    case Abstract() => s"$indent TopBoolean"
    case _ => throw new IllegalArgumentException("boolean lattice error")
  }

}
