package tapy.lattices

import java.lang.Double
import tapy.dfa._

object FloatLattice {
  sealed trait Elt extends MergeLattice.Elt
  
  case class Concrete(l:Double) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
} 

class FloatLattice extends Lattice[FloatLattice.Elt] {
  def top: FloatLattice.Elt = FloatLattice.Abstract()
  def bottom: FloatLattice.Elt = FloatLattice.Bottom()
  
  // a >= b
  def compare(a: FloatLattice.Elt, b: FloatLattice.Elt) = (a, b) match {
    case (FloatLattice.Abstract(), _)  => true
    case (_, FloatLattice.Bottom()) => true
    case (FloatLattice.Concrete(i),FloatLattice.Concrete(j)) => (i.equals(j))
    case _ => false
  }

  def leastUpperBound(a: FloatLattice.Elt, b: FloatLattice.Elt) = (a, b) match {
    case (FloatLattice.Abstract(), _) =>                           FloatLattice.Abstract()
    case (_, FloatLattice.Abstract()) =>                           FloatLattice.Abstract()

    case (FloatLattice.Bottom(), FloatLattice.Bottom()) =>  FloatLattice.Bottom()

    case (FloatLattice.Concrete(i),FloatLattice.Bottom()) => FloatLattice.Concrete(i)
    case (FloatLattice.Bottom(),FloatLattice.Concrete(i)) => FloatLattice.Concrete(i)

    case (FloatLattice.Concrete(i),FloatLattice.Concrete(j)) => if (i.equals(j)) FloatLattice.Concrete(j) else FloatLattice.Abstract()

    case (_, _) =>  FloatLattice.Abstract()
  }

  def greatestLowerBound(a: FloatLattice.Elt, b: FloatLattice.Elt) = (a, b) match {
    case (FloatLattice.Bottom(), _) =>                        FloatLattice.Bottom()
    case (_, FloatLattice.Bottom()) =>                        FloatLattice.Bottom()

    case (FloatLattice.Abstract(), FloatLattice.Abstract()) =>  FloatLattice.Abstract()


    case (FloatLattice.Concrete(i),FloatLattice.Abstract()) => FloatLattice.Concrete(i)
    case (FloatLattice.Abstract(),FloatLattice.Concrete(i)) => FloatLattice.Concrete(i)

    case (FloatLattice.Concrete(i),FloatLattice.Concrete(j)) => if (i.equals(j)) FloatLattice.Concrete(j) else FloatLattice.Bottom()

    case (_, _) =>  FloatLattice.Bottom()
  }
}
