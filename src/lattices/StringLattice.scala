package tapy.lattices

import tapy.dfa._

sealed trait StringElt

object StringLattice extends Lattice[StringElt] {
  type Elt = StringElt
  
  case class Bottom() extends Elt
  case class Concrete(s: String) extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  // a >= b
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Abstract(), _) => true
    case (Concrete(_), Abstract()) => false
    case (Concrete(s1), Concrete(s2)) => s1 == s2
    case (Concrete(_), Bottom()) => true
    case (Bottom(), Abstract()) => false
    case (Bottom(), Bottom()) => true
    case (Bottom(), Concrete(_)) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = return (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Concrete(s1), Concrete(s2)) => if (s1 == s2) a else Abstract()
    case (Concrete(_), Bottom()) => a
    case (Bottom(), Concrete(_)) => b
    case (Bottom(), Bottom()) => Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = return (a, b) match {
    case (Abstract(), _) => b
    case (_, Abstract()) => a
    case (Concrete(s1), Concrete(s2)) => if (s1 == s2) a else Bottom()
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case _ => throw new IllegalArgumentException()
  }
}