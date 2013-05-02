package tapy.lattices

import java.math.BigInteger
import tapy.dfa._

object LongLattice {
  sealed trait Elt
  case class Concrete(l:BigInteger) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
} 

class LongLattice extends Lattice[LongLattice.Elt] {
  def top: LongLattice.Elt = LongLattice.Abstract()
  def bottom: LongLattice.Elt = LongLattice.Bottom()
  
  // a >= b
  def compare(a: LongLattice.Elt, b: LongLattice.Elt) = (a, b) match {
    case (LongLattice.Abstract(), _)  => true
    case (_, LongLattice.Bottom()) => true
    case (LongLattice.Concrete(i),LongLattice.Concrete(j)) => (i.equals(j))
    case _ => false
  }

  def leastUpperBound(a: LongLattice.Elt, b: LongLattice.Elt) = (a, b) match {
    case (LongLattice.Abstract(), _) =>                           LongLattice.Abstract()
    case (_, LongLattice.Abstract()) =>                           LongLattice.Abstract()

    case (LongLattice.Bottom(), LongLattice.Bottom()) =>  LongLattice.Bottom()

    case (LongLattice.Concrete(i),LongLattice.Bottom()) => LongLattice.Concrete(i)
    case (LongLattice.Bottom(),LongLattice.Concrete(i)) => LongLattice.Concrete(i)

    case (LongLattice.Concrete(i),LongLattice.Concrete(j)) => if (i.equals(j)) LongLattice.Concrete(j) else LongLattice.Abstract()

    case (_, _) =>  LongLattice.Abstract()
  }

  def greatestLowerBound(a: LongLattice.Elt, b: LongLattice.Elt) = (a, b) match {
    case (LongLattice.Bottom(), _) =>                        LongLattice.Bottom()
    case (_, LongLattice.Bottom()) =>                        LongLattice.Bottom()

    case (LongLattice.Abstract(), LongLattice.Abstract()) =>  LongLattice.Abstract()


    case (LongLattice.Concrete(i),LongLattice.Abstract()) => LongLattice.Concrete(i)
    case (LongLattice.Abstract(),LongLattice.Concrete(i)) => LongLattice.Concrete(i)

    case (LongLattice.Concrete(i),LongLattice.Concrete(j)) => if (i.equals(j)) LongLattice.Concrete(j) else LongLattice.Bottom()

    case (_, _) =>  LongLattice.Bottom()
  }
}
