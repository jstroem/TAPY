package tapy.dfa

class ProductLattice[A, B](latticeA: Lattice[A], latticeB: Lattice[B]) extends Lattice[(A, B)] {
  type Elt = (A, B)
  
  def top: (A, B) = (latticeA.top, latticeB.top)
  def bottom: (A, B) = (latticeA.bottom, latticeB.bottom)

  def compare(a: (A, B), b: (A, B)) = {
    val fst = latticeA.compare(a._1, b._1)
    val snd = latticeB.compare(a._2, b._2)
    fst && snd
  }

  def leastUpperBound(a: (A, B), b: (A, B)) = {
    val fst = latticeA.leastUpperBound(a._1, b._1)
    val snd = latticeB.leastUpperBound(a._2, b._2)
    (fst, snd)
  }

  def greatestLowerBound(a: (A, B), b: (A, B)) = {
    val fst = latticeA.greatestLowerBound(a._1, b._1)
    val snd = latticeB.greatestLowerBound(a._2, b._2)
    (fst, snd)
  }

  def eltToString(elt: (A, B), indent: String) : String = {
    val (a, b) = elt
    val sa = latticeA.eltToString(a, indent)
    val sb = latticeB.eltToString(b, indent)
    sa +
    indent + "  X\n" +
    sb
  }
}
