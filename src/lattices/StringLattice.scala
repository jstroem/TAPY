package tapy.lattices

import tapy.dfa._

object StringLattice {
  sealed trait Elt

  case class Bottom() extends Elt
  case class Concrete(s: String) extends Elt
  case class Abstract() extends Elt
}

class StringLattice extends Lattice[StringLattice.Elt] {
  def top: StringLattice.Elt = StringLattice.Abstract()
  def bottom: StringLattice.Elt = StringLattice.Bottom()
  
  // a >= b
  def compare(a: StringLattice.Elt, b: StringLattice.Elt): Boolean = return (a, b) match {
    case (StringLattice.Abstract(), _) => true
    
    case (StringLattice.Concrete(_), StringLattice.Abstract()) => false
    case (StringLattice.Concrete(s1), StringLattice.Concrete(s2)) => s1 == s2
    case (StringLattice.Concrete(_), StringLattice.Bottom()) => true
    
    case (StringLattice.Bottom(), StringLattice.Abstract()) => false
    case (StringLattice.Bottom(), StringLattice.Bottom()) => true
    case (StringLattice.Bottom(), StringLattice.Concrete(_)) => false
  }
  
  def leastUpperBound(a: StringLattice.Elt, b: StringLattice.Elt): StringLattice.Elt = {
    return (a, b) match {
      case (StringLattice.Abstract(), _) => StringLattice.Abstract()
      case (_, StringLattice.Abstract()) => StringLattice.Abstract()
      case (StringLattice.Concrete(s1), StringLattice.Concrete(s2)) => if (s1 == s2) a else StringLattice.Abstract()
      case (StringLattice.Concrete(_), StringLattice.Bottom()) => a
      case (StringLattice.Bottom(), StringLattice.Concrete(_)) => b
      case (StringLattice.Bottom(), StringLattice.Bottom()) => StringLattice.Bottom()
    }
  }
  
  def greatestLowerBound(a: StringLattice.Elt, b: StringLattice.Elt): StringLattice.Elt = {
    return (a, b) match {
      case (StringLattice.Abstract(), _) => b
      case (_, StringLattice.Abstract()) => a
      case (StringLattice.Concrete(s1), StringLattice.Concrete(s2)) => if (s1 == s2) a else StringLattice.Bottom()
      case (StringLattice.Bottom(), _) => StringLattice.Bottom()
      case (_, StringLattice.Bottom()) => StringLattice.Bottom()
    }
  }
}