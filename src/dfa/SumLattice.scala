package tapy.dfa

object SumLattice {
  trait Elt
  
  case class Top() extends Elt
  case class Bottom() extends Elt
}

/*
 * SumLattice combines two lattices into one by placing them in parallel and adding a new top element.
 * The bottom element is both the bottom element of latticeA and latticeB. 
 */
class SumLattice[A <: SumLattice.Elt, B <: SumLattice.Elt](latticeA: Lattice[A], latticeB: Lattice[B]) extends Lattice[SumLattice.Elt] {
  def top: SumLattice.Elt = SumLattice.Top()
  def bottom: SumLattice.Elt = SumLattice.Bottom()
  
  def compare(a: SumLattice.Elt, b: SumLattice.Elt): Boolean = {
    if (a == SumLattice.Top() || b == SumLattice.Bottom())
      return true
    
    return (a, b) match {
      case (a: A @unchecked, b: A @unchecked) => latticeA.compare(a, b)
      case (a: B @unchecked, b: B @unchecked) => latticeB.compare(a, b)
      case (a: A @unchecked, b: B @unchecked) => b == latticeB.bottom
      case (a: B @unchecked, b: A @unchecked) => b == latticeA.bottom
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
      
  def leastUpperBound(a: SumLattice.Elt, b: SumLattice.Elt): SumLattice.Elt = {
    if (a == SumLattice.Top() || b == SumLattice.Top())
      return SumLattice.Top()
    
    return (a, b) match {
      case (a: A @unchecked, b: A @unchecked) => latticeA.leastUpperBound(a, b)
      case (a: B @unchecked, b: B @unchecked) => latticeB.leastUpperBound(a, b)
      case (a: A @unchecked, b: B @unchecked) => SumLattice.Top()
      case (a: B @unchecked, b: A @unchecked) => SumLattice.Top()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }

  def greatestLowerBound(a: SumLattice.Elt, b: SumLattice.Elt): SumLattice.Elt = {
    if (a == SumLattice.Bottom() || b == SumLattice.Bottom())
      return SumLattice.Bottom()
    
    return (a, b) match {
      case (a: A @unchecked, b: A @unchecked) => latticeA.greatestLowerBound(a, b)
      case (a: B @unchecked, b: B @unchecked) => latticeB.greatestLowerBound(a, b)
      case (a: A @unchecked, b: B @unchecked) => SumLattice.Bottom()
      case (a: B @unchecked, b: A @unchecked) => SumLattice.Bottom()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
}