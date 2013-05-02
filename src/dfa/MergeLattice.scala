package tapy.dfa

object MergeLattice {
  trait Elt
  
  case class Top() extends Elt
  case class Bottom() extends Elt
}

/*
 * MergeLattice combines two lattices into one by placing them in parallel and adding a new top element.
 * The bottom element is both the bottom element of latticeA and latticeB. 
 */
class MergeLattice[A <: MergeLattice.Elt, B <: MergeLattice.Elt](latticeA: Lattice[A], latticeB: Lattice[B]) extends Lattice[MergeLattice.Elt] {
  def top: MergeLattice.Elt = MergeLattice.Top()
  def bottom: MergeLattice.Elt = MergeLattice.Bottom()

  def compare(a: MergeLattice.Elt, b: MergeLattice.Elt): Boolean = {
    if (a == MergeLattice.Top() || b == MergeLattice.Bottom())
      return true
    
    return (a, b) match {
      case (a: A, b: A) => latticeA.compare(a, b)
      case (a: B, b: B) => latticeB.compare(a, b)
      case (a: A, b: B) => b == latticeB.bottom
      case (a: B, b: A) => b == latticeA.bottom
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
      
  def leastUpperBound(a: MergeLattice.Elt, b: MergeLattice.Elt): MergeLattice.Elt = {
    if (a == MergeLattice.Top() || b == MergeLattice.Top())
      return MergeLattice.Top()
    
    return (a, b) match {
      case (a: A, b: A) => latticeA.leastUpperBound(a, b)
      case (a: B, b: B) => latticeB.leastUpperBound(a, b)
      case (a: A, b: B) => MergeLattice.Top()
      case (a: B, b: A) => MergeLattice.Top()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }

  def greatestLowerBound(a: (A, B), b: (A, B)): MergeLattice.Elt = {
    if (a == MergeLattice.Bottom() || b == MergeLattice.Bottom())
      return MergeLattice.Bottom()
    
    return (a, b) match {
      case (a: A, b: A) => latticeA.greatestLowerBound(a, b)
      case (a: B, b: B) => latticeB.greatestLowerBound(a, b)
      case (a: A, b: B) => MergeLattice.Bottom()
      case (a: B, b: A) => MergeLattice.Bottom()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
}
