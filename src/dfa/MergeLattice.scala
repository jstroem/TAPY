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
      case (a: A @unchecked, b: A @unchecked) => latticeA.compare(a, b)
      case (a: B @unchecked, b: B @unchecked) => latticeB.compare(a, b)
      case (a: A @unchecked, b: B @unchecked) => b == latticeB.bottom
      case (a: B @unchecked, b: A @unchecked) => b == latticeA.bottom
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
      
  def leastUpperBound(a: MergeLattice.Elt, b: MergeLattice.Elt): MergeLattice.Elt = {
    if (a == MergeLattice.Top() || b == MergeLattice.Top())
      return MergeLattice.Top()
    
    return (a, b) match {
      case (a: A @unchecked, b: A @unchecked) => latticeA.leastUpperBound(a, b)
      case (a: B @unchecked, b: B @unchecked) => latticeB.leastUpperBound(a, b)
      case (a: A @unchecked, b: B @unchecked) => MergeLattice.Top()
      case (a: B @unchecked, b: A @unchecked) => MergeLattice.Top()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }

  def greatestLowerBound(a: MergeLattice.Elt, b: MergeLattice.Elt): MergeLattice.Elt = {
    if (a == MergeLattice.Bottom() || b == MergeLattice.Bottom())
      return MergeLattice.Bottom()
    
    return (a, b) match {
      case (a: A @unchecked, b: A @unchecked) => latticeA.greatestLowerBound(a, b)
      case (a: B @unchecked, b: B @unchecked) => latticeB.greatestLowerBound(a, b)
      case (a: A @unchecked, b: B @unchecked) => MergeLattice.Bottom()
      case (a: B @unchecked, b: A @unchecked) => MergeLattice.Bottom()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
}