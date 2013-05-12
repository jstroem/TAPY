package tapy.dfa

class PowerSubSetLattice[A] extends Lattice[Set[A]] {
  type Elt = Set[A]

  def top: Set[A] = null // s
  def bottom: Set[A] = Set[A]()
  
  def compare(a: Set[A], b: Set[A]) = a.subsetOf(b)
  def leastUpperBound(a: Set[A], b: Set[A]) = if (a == null || b == null) null else a ++ b
  def greatestLowerBound(a: Set[A], b: Set[A]) = if (a == null) b else if (b == null) a else a & b

  def eltToString(elt: Set[A], indent: String): String = {
    val elementsString =
      if (elt == null)
        "<TOP>"
      else
        elt.foldLeft ("") ((s: String, e: A) => {
          val eString = e.toString
          if (s == "")
            eString
          else
            s"$s, $eString"
        })
    s"${indent}{$elementsString}\n"
  }
}