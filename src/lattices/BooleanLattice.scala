package tapy.lattices

import tapy.dfa._

sealed trait BooleanElt

class BooleanLattice extends Lattice[BooleanElt] {
  type Elt = BooleanElt
  
  case class True() extends Elt
  case class False() extends Elt
  case class Bottom() extends Elt
  case class Bool() extends Elt
  
  def top: Elt = Bool()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Bool(), _)  => true
    case (_, Bottom()) => true
    case (True(), True()) => true
    case (False(), False()) => true
    case _ => false
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Bool(), _) => Bool()
    case (_, Bool()) => Bool()

    case (True(), True()) => True()
    case (False(), False()) => False()


    case (False(), Bottom()) => False()
    case (True(), Bottom()) => True()

    case (Bottom(), False()) => False()
    case (Bottom(), True()) => True()

    case (Bottom(), Bottom()) => Bottom()

    case (_, _) =>  Bool()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()

    case (True(), True()) => True()
    case (False(), False()) => False()


    case (False(), Bool()) => False()
    case (True(), Bool()) => True()

    case (Bool(), False()) => False()
    case (Bool(), True()) => True()

    case (Bool(), Bool()) => Bool()

    case (_, _) =>  Bottom()
  }
}
