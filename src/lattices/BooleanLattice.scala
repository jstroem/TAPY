package tapy.lattices

import tapy.dfa._

object BooleanLattice {
  sealed trait Elt
  
  case class True() extends Elt
  case class False() extends Elt
  case class Bottom() extends Elt
  case class Bool() extends Elt
}

class BooleanLattice extends Lattice[BooleanLattice.Elt] {
  def top: BooleanLattice.Elt = BooleanLattice.Bool()
  def bottom: BooleanLattice.Elt = BooleanLattice.Bottom()
  
  // a >= b
  def compare(a: BooleanLattice.Elt, b: BooleanLattice.Elt) = (a, b) match {
    case (BooleanLattice.Bool(), _)  => true
    case (_, BooleanLattice.Bottom()) => true
    case (BooleanLattice.True(), BooleanLattice.True()) => true
    case (BooleanLattice.False(), BooleanLattice.False()) => true
    case _ => false
  }

  def leastUpperBound(a: BooleanLattice.Elt, b: BooleanLattice.Elt) = (a, b) match {
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

  def greatestLowerBound(a: BooleanLattice.Elt, b: BooleanLattice.Elt) = (a, b) match {
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
