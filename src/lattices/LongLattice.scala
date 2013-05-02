package tapy.lattices

import java.math.BigInteger
import tapy.dfa._

object LongLattice {
  sealed trait Long
  case class L(l:BigInteger) extends Long
  case class Bottom() extends Long
  case class Top() extends Long
} 

class LongLattice extends Lattice[LongLattice.Long] {
  def top: LongLattice.Long = LongLattice.Top()
  def bottom: LongLattice.Long = LongLattice.Bottom()
  
  // a >= b
  def compare(a: LongLattice.Long, b: LongLattice.Long) = (a, b) match {
    case (LongLattice.Top(), _)  => true
    case (_, LongLattice.Bottom()) => true
    case _ => false
  }

  def leastUpperBound(a: LongLattice.Long, b: LongLattice.Long) = (a, b) match {
    case (LongLattice.Top(), _) =>                           LongLattice.Top()
    case (_, LongLattice.Top()) =>                           LongLattice.Top()

    case (LongLattice.Bottom(), LongLattice.Bottom()) =>  LongLattice.Bottom()

    case (_, _) =>  LongLattice.Top()
  }

  def greatestLowerBound(a: LongLattice.Long, b: LongLattice.Long) = (a, b) match {
    case (LongLattice.Bottom(), _) =>                        LongLattice.Bottom()
    case (_, LongLattice.Bottom()) =>                        LongLattice.Bottom()

    case (LongLattice.Top(), LongLattice.Top()) =>  LongLattice.Top()

    case (_, _) =>  LongLattice.Bottom()
  }
}
