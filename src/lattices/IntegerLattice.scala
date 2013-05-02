package tapy.lattices

import tapy.dfa._

/*  Assuming that the python program is running on a 32 bit computer.
    This limits integers in python to -2**31-1 <= x <= 2**32-1. */

sealed trait IntegerElt extends SumLattice.Elt

object IntegerLattice extends Lattice[IntegerElt] {
  type Elt = IntegerElt
  
  case class Concrete(i: Int) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i == j)
    case _ => false
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
}
