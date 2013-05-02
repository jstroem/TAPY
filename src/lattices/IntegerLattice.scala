package tapy.lattices

import tapy.dfa._


/*  Assuming that the python program is running on a 32 bit computer.
    This limits integers in python to -2**31-1 <= x <= 2**32-1. */
object IntegerLattice {
  sealed trait Integer
  case class I(i:Int) extends Integer
  case class Bottom() extends Integer
  case class Int() extends Integer
} 

class IntegerLattice extends Lattice[IntegerLattice.Integer] {
  def top: IntegerLattice.Integer = IntegerLattice.Int()
  def bottom: IntegerLattice.Integer = IntegerLattice.Bottom()
  
  // a >= b
  def compare(a: IntegerLattice.Integer, b: IntegerLattice.Integer) = (a, b) match {
    case (IntegerLattice.Int(), _)  => true
    case (_, IntegerLattice.Bottom()) => true
    case _ => false
  }

  def leastUpperBound(a: IntegerLattice.Integer, b: IntegerLattice.Integer) = (a, b) match {
    case (IntegerLattice.Int(), _) =>                           IntegerLattice.Int()
    case (_, IntegerLattice.Int()) =>                           IntegerLattice.Int()

    case (IntegerLattice.Bottom(), IntegerLattice.Bottom()) =>  IntegerLattice.Bottom()

    case (_, _) =>  IntegerLattice.Int()
  }

  def greatestLowerBound(a: IntegerLattice.Integer, b: IntegerLattice.Integer) = (a, b) match {
    case (IntegerLattice.Bottom(), _) =>                        IntegerLattice.Bottom()
    case (_, IntegerLattice.Bottom()) =>                        IntegerLattice.Bottom()

    case (IntegerLattice.Int(), IntegerLattice.Int()) =>  IntegerLattice.Int()

    case (_, _) =>  IntegerLattice.Bottom()
  }
}
