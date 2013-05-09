package tapy.lattices

import java.math.BigInteger
import tapy.dfa._

sealed trait LongElt

object LongLattice extends Lattice[LongElt] {
  type Elt = LongElt
  
  case class Concrete(l:BigInteger) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i.equals(j))
    case _ => false
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Bottom(), Bottom()) => Bottom()
    case (Concrete(i),Bottom()) => Concrete(i)
    case (Bottom(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Abstract()
    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case (Abstract(), Abstract()) => Abstract()
    case (Concrete(i),Abstract()) => Concrete(i)
    case (Abstract(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Bottom()
    case (_, _) =>  Bottom()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent[Long $l]\n"
    case Bottom() => s"$indent[Not a Long (Bottom)]\n"
    case Abstract() => s"$indent[Any long (Top)]\n"
    case _ => throw new IllegalArgumentException("LongLattice pattern match error")
  }
  
  /* Element utility functions */
  
  def elementToFloat(el: LongLattice.Elt): FloatLattice.Elt = el match {
    case Abstract() => FloatLattice.Abstract()
    case Concrete(i) => FloatLattice.Concrete(i.floatValue())
    case Bottom() => FloatLattice.Bottom()
  }
  
  def elementToComplex(el: LongLattice.Elt): ComplexLattice.Elt = el match {
    case Abstract() => (FloatLattice.Abstract(), FloatLattice.Concrete(0))
    case Concrete(i) => (FloatLattice.Concrete(i.floatValue()), FloatLattice.Concrete(0))
    case Bottom() => (FloatLattice.Bottom(), FloatLattice.Bottom())
  }
}