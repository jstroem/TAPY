package tapy.lattices

import java.lang.Double
import tapy.dfa._

sealed trait FloatElt

object FloatLattice extends Lattice[FloatElt] {
  type Elt = FloatElt
  
  case class Concrete(l:Double) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i.equals(j))
    case _ => throw new IllegalArgumentException()
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Bottom(), Bottom()) =>  Bottom()
    case (Concrete(i),Bottom()) => Concrete(i)
    case (Bottom(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Abstract()
    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case (Abstract(), Abstract()) =>  Abstract()
    case (Concrete(i),Abstract()) => Concrete(i)
    case (Abstract(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Bottom()
    case (_, _) =>  Bottom()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent[Float: $l]\n"
    case Bottom() => s"$indent[Not a Float (Bottom)]\n"
    case Abstract() => s"$indent[Any Float (Top)]\n"
    case _ => throw new IllegalArgumentException("FloatLattice pattern match error")
  }
  
  /* Element utility functions */
  
  def elementToComplex(el: FloatLattice.Elt): ComplexLattice.Elt = el match {
    case Abstract() => (Abstract(), Concrete(0))
    case Concrete(i) => (Concrete(i), Concrete(0))
    case Bottom() => (Bottom(), Bottom())
  }
}
