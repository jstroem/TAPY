package tapy.lattices

import java.math.BigInteger
import tapy.dfa._

sealed trait LongElt

class LongLattice extends Lattice[LongElt] {
  type Elt = LongElt
  
  case class Concrete(l:BigInteger) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i.equals(j))
    case _ => false
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) =>                           Abstract()
    case (_, Abstract()) =>                           Abstract()

    case (Bottom(), Bottom()) =>  Bottom()

    case (Concrete(i),Bottom()) => Concrete(i)
    case (Bottom(),Concrete(i)) => Concrete(i)

    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Abstract()

    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) =>                        Bottom()
    case (_, Bottom()) =>                        Bottom()

    case (Abstract(), Abstract()) =>  Abstract()


    case (Concrete(i),Abstract()) => Concrete(i)
    case (Abstract(),Concrete(i)) => Concrete(i)

    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Bottom()

    case (_, _) =>  Bottom()
  }
}
