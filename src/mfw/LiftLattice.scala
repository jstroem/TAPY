package tapy.sign

import tapy.mfw._

class LiftLattice[T](lattice: Lattice[T]) extends Lattice[Option[T]] {

  def top: Option[T] = Some(lattice.top)
  def bottom: Option[T] = None

  def compare(a: Option[T], b: Option[T]) = (a, b) match {
    case (None, None) => true
    case (None, Some (b)) => false
    case (Some (a), None) => true
    case (Some (a), Some (b)) => lattice.compare(a, b)
  }

  def leastUpperBound(a: Option[T], b: Option[T]) = (a, b) match {
    case (None, None) => None
    case (None, Some (b)) => Some (b)
    case (Some (a), None) => Some (a)
    case (Some (a), Some (b)) => Some (lattice.leastUpperBound(a, b))
  }

  def greatestLowerBound(a: Option[T], b: Option[T]) = (a, b) match {
    case (None, None) => None
    case (None, Some (b)) => None
    case (Some (a), None) => None
    case (Some (a), Some (b)) => Some (lattice.greatestLowerBound(a, b))
  }
}
