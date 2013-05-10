package tapy.dfa

sealed trait PowerSubSetElement

class PowerSubSetTopLattice[A] extends Lattice[PowerSubSetElement] {
  type Elt = PowerSubSetElement

  case class Top() extends PowerSubSetElement
  case class Concrete(s: Set[A]) extends PowerSubSetElement

  def top: PowerSubSetElement = Top()
  def bottom: PowerSubSetElement = Concrete(Set[A]())
  
  def compare(a: PowerSubSetElement, b: PowerSubSetElement) = (a, b) match {
    case (Concrete(a), Concrete(b)) => a.subsetOf(b)
    case (Concrete(a), _ ) => false
    case _ => true
  }

  def leastUpperBound(a: PowerSubSetElement, b: PowerSubSetElement) = (a, b) match {
    case (Concrete(a), Concrete(b)) => Concrete(a++b)
    case _ => Top()
  }

  def greatestLowerBound(a: PowerSubSetElement, b: PowerSubSetElement) = (a, b) match {
    case (Concrete(a), Concrete(b)) => Concrete(a & b)
    case (Concrete(a), Top()) => Concrete(a)
    case (Top(), Concrete(a)) => Concrete(a)
    case _ => Top()
  }

  def eltToString(e: PowerSubSetElement, indent: String): String = e match {
    case Concrete(elt) => {
      val elementsString = elt.foldLeft ("") ((s: String, e: A) => {
        val eString = e.toString
        if (s == "")
          eString
        else
          s"$s, $eString"
      })
      s"${indent}{$elementsString}\n"
    }
    case _ => s"${indent}[Top POWERSET]"
  }

  def eltSize(e: PowerSubSetElement): Int = e match {
    case Concrete(s) => s.size
    case _ => 2000000000
  }

  def makeElement(s: Set[A]): PowerSubSetElement = Concrete(s)
}

