package tapy.dfa

case class EmptyElt

object EmptyLattice extends Lattice[EmptyElt] {
  type Elt = EmptyElt
  
  def Empty = EmptyElt
  
  def top: Elt = Empty()
  def bottom: Elt = Empty()
  
  def compare(a: Elt, b: Elt) = true
  def leastUpperBound(a: Elt, b: Elt) = a
  def greatestLowerBound(a: Elt, b: Elt) = a
}