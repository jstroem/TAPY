package tapy.dfa

object EmptyLattice {
  case class Empty()
}

class EmptyLattice() extends Lattice[EmptyLattice.Empty] {
  def top: EmptyLattice.Empty = EmptyLattice.Empty()
  def bottom: EmptyLattice.Empty = EmptyLattice.Empty()
  
  def compare(a: EmptyLattice.Empty, b: EmptyLattice.Empty) = true
  def leastUpperBound(a: EmptyLattice.Empty, b: EmptyLattice.Empty) = a
  def greatestLowerBound(a: EmptyLattice.Empty, b: EmptyLattice.Empty) = a
}