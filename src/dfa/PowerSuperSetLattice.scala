package tapy.dfa

abstract class PowerSuperSetLattice[A](s: Set[A]) extends Lattice[Set[A]] {
  def top: Set[A] = Set[A]()
  def bottom: Set[A] = s
  
  def compare(a: Set[A], b: Set[A]) = b.subsetOf(a)
  def leastUpperBound(a: Set[A], b: Set[A]) = a & b
  def greatestLowerBound(a: Set[A], b: Set[A]) = a ++ b
}
