package tapy.dfa

class PowerSubSetLattice[A](s: Set[A]) extends Lattice[Set[A]] {
  def top: Set[A] = s
  def bottom: Set[A] = Set[A]()
  def compare(a: Set[A], b: Set[A]) = a.subsetOf(b)
  def leastUpperBound(a: Set[A], b: Set[A]) = a ++ b
  def greatestLowerBound(a: Set[A], b: Set[A]) = a & b
}