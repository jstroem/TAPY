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
    case Concrete(l) => s"$indent[Boolean: $l]\n"
    case Bottom() => s"$indent[Not a Boolean (Bottom)]\n"
    case Abstract() => s"$indent[Any String (Top)]\n"
    case _ => throw new IllegalArgumentException("BooleanLattice pattern match error")
  }
  
  /* Element utility functions */
  
  def elementToInteger(el: BooleanLattice.Elt): IntegerLattice.Elt = el match {
    case Abstract() => IntegerLattice.Abstract()
    case Concrete(b) => IntegerLattice.Concrete(if (b) 1 else 0)
    case Bottom() => IntegerLattice.Bottom()
  }
  
  def elementToFloat(el: BooleanLattice.Elt): FloatLattice.Elt = el match {
    case Abstract() => FloatLattice.Abstract()
    case Concrete(b) => FloatLattice.Concrete(if (b) 1 else 0)
    case Bottom() => FloatLattice.Bottom()
  }
  
  def elementToLong(el: BooleanLattice.Elt): LongLattice.Elt = el match {
    case Abstract() => LongLattice.Abstract()
    case Concrete(b) => LongLattice.Concrete(java.math.BigInteger.valueOf(if (b) 1 else 0))
    case Bottom() => LongLattice.Bottom()
  }
  
  def elementToComplex(el: BooleanLattice.Elt): ComplexLattice.Elt = el match {
    case Abstract() => (FloatLattice.Abstract(), FloatLattice.Concrete(0))
    case Concrete(b) => (FloatLattice.Concrete(if (b) 1 else 0), FloatLattice.Concrete(0))
    case Bottom() => (FloatLattice.Bottom(), FloatLattice.Bottom())
  }
}
