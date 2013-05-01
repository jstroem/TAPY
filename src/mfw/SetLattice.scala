package tapy.sign

import tapy.mfw._

class SetLattice[A](s: Set[A]) extends Lattice[Set[A]] {
  def top: Set[A] = s
  def bottom: Set[A] = Set[A]()
  def compare(a: Set[A], b: Set[A]) = b.subsetOf(a)
  def leastUpperBound(a: Set[A], b: Set[A]) = a ++ b
  def greatestLowerBound(a: Set[A], b: Set[A]) = a & b
}
