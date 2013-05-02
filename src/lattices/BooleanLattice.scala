package tapy.lattices

import tapy.dfa._

object BooleanLattice {
  sealed trait Boolean
  case class True() extends Boolean
  case class False() extends Boolean
  case class Bottom() extends Boolean
  case class Bool() extends Boolean
}

class BooleanLattice extends Lattice[BooleanLattice.Boolean] {
  def top: BooleanLattice.Boolean = BooleanLattice.Bool()
  def bottom: BooleanLattice.Boolean = BooleanLattice.Bottom()
  
  // a >= b
  def compare(a: BooleanLattice.Boolean, b: BooleanLattice.Boolean) = (a, b) match {
    case (BooleanLattice.Bool(), _)  => true
    case (_, BooleanLattice.Bottom()) => true
    case (BooleanLattice.True(), BooleanLattice.True()) => true
    case (BooleanLattice.False(), BooleanLattice.False()) => true
    case _ => false
  }

  def leastUpperBound(a: BooleanLattice.Boolean, b: BooleanLattice.Boolean) = (a, b) match {
    case (BooleanLattice.Bool(), _) =>                        BooleanLattice.Bool()
    case (_, BooleanLattice.Bool()) =>                        BooleanLattice.Bool()

    case (BooleanLattice.True(), BooleanLattice.True()) =>    BooleanLattice.True()
    case (BooleanLattice.False(), BooleanLattice.False()) =>  BooleanLattice.False()


    case (BooleanLattice.False(), BooleanLattice.Bottom()) => BooleanLattice.False()
    case (BooleanLattice.True(), BooleanLattice.Bottom()) =>  BooleanLattice.True()

    case (BooleanLattice.Bottom(), BooleanLattice.False()) => BooleanLattice.False()
    case (BooleanLattice.Bottom(), BooleanLattice.True()) =>  BooleanLattice.True()

    case (BooleanLattice.Bottom(), BooleanLattice.Bottom()) =>  BooleanLattice.Bottom()

    case (_, _) =>  BooleanLattice.Bool()
  }

  def greatestLowerBound(a: BooleanLattice.Boolean, b: BooleanLattice.Boolean) = (a, b) match {
    case (BooleanLattice.Bottom(), _) =>                        BooleanLattice.Bottom()
    case (_, BooleanLattice.Bottom()) =>                        BooleanLattice.Bottom()

    case (BooleanLattice.True(), BooleanLattice.True()) =>    BooleanLattice.True()
    case (BooleanLattice.False(), BooleanLattice.False()) =>  BooleanLattice.False()


    case (BooleanLattice.False(), BooleanLattice.Bool()) => BooleanLattice.False()
    case (BooleanLattice.True(), BooleanLattice.Bool()) =>  BooleanLattice.True()

    case (BooleanLattice.Bool(), BooleanLattice.False()) => BooleanLattice.False()
    case (BooleanLattice.Bool(), BooleanLattice.True()) =>  BooleanLattice.True()

    case (BooleanLattice.Bool(), BooleanLattice.Bool()) =>  BooleanLattice.Bool()

    case (_, _) =>  BooleanLattice.Bottom()
  }
}
