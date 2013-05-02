package tapy.dfa

object MergeLattice {
  trait Elt
  
  case class Top() extends Elt
  case class Bottom() extends Elt
  case class ConcreteElt[T](elt: T) extends Elt
}

/*
 * MergeLattice combines two lattices into one by placing them in parallel and adding a new top element.
 * The bottom element is both the bottom element of latticeA and latticeB. 
 */
class MergeLattice[A <: MergeLattice.ConcreteElt[A], B <: MergeLattice.ConcreteElt[B]](latticeA: Lattice[A], latticeB: Lattice[B]) extends Lattice[MergeLattice.Elt] {
  def top: MergeLattice.Elt = MergeLattice.Top()
  def bottom: MergeLattice.Elt = MergeLattice.Bottom()

  def compare(a: MergeLattice.Elt, b: MergeLattice.Elt): Boolean = {
    if (a == MergeLattice.Top() || b == MergeLattice.Bottom())
      return true
    
    return (a, b) match {
      case (a: MergeLattice.ConcreteElt[A], b: MergeLattice.ConcreteElt[A]) => latticeA.compare(a.elt, b.elt)
      case (a: MergeLattice.ConcreteElt[B], b: MergeLattice.ConcreteElt[B]) => latticeB.compare(a.elt, b.elt)
      case (a: MergeLattice.ConcreteElt[A], b: MergeLattice.ConcreteElt[B]) => b.elt == latticeB.bottom
      case (a: MergeLattice.ConcreteElt[B], b: MergeLattice.ConcreteElt[A]) => b.elt == latticeA.bottom
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
      
  def leastUpperBound(a: MergeLattice.Elt, b: MergeLattice.Elt): MergeLattice.Elt = {
    if (a == MergeLattice.Top() || b == MergeLattice.Top())
      return MergeLattice.Top()
    
    return (a, b) match {
      case (a: MergeLattice.ConcreteElt[A], b: MergeLattice.ConcreteElt[A]) => latticeA.leastUpperBound(a.elt, b.elt)
      case (a: MergeLattice.ConcreteElt[B], b: MergeLattice.ConcreteElt[B]) => latticeB.leastUpperBound(a.elt, b.elt)
      case (a: MergeLattice.ConcreteElt[A], b: MergeLattice.ConcreteElt[B]) => MergeLattice.Top()
      case (a: MergeLattice.ConcreteElt[B], b: MergeLattice.ConcreteElt[A]) => MergeLattice.Top()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }

  def greatestLowerBound(a: MergeLattice.Elt, b: MergeLattice.Elt): MergeLattice.Elt = {
    if (a == MergeLattice.Bottom() || b == MergeLattice.Bottom())
      return MergeLattice.Bottom()
    
    return (a, b) match {
      case (a: MergeLattice.ConcreteElt[A], b: MergeLattice.ConcreteElt[A]) => latticeA.greatestLowerBound(a.elt, b.elt)
      case (a: MergeLattice.ConcreteElt[B], b: MergeLattice.ConcreteElt[B]) => latticeB.greatestLowerBound(a.elt, b.elt)
      case (a: MergeLattice.ConcreteElt[A], b: MergeLattice.ConcreteElt[B]) => MergeLattice.Bottom()
      case (a: MergeLattice.ConcreteElt[B], b: MergeLattice.ConcreteElt[A]) => MergeLattice.Bottom()
      case _ => throw new IllegalArgumentException("Provided arguments are not part of this merge lattice.")
    }
  }
}
