package tapy.dfa

sealed trait FlatElt

class FlatLattice[T] extends Lattice[FlatElt] {
  type Elt = FlatElt
  
  case class Top() extends Elt
  case class Singleton(elt: T) extends Elt
  case class Bottom() extends Elt
  
  def top: Elt = Top()
  def bottom: Elt = Bottom()

  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Top(), _) => true
    case (Singleton(_), Top()) => false
    case (Singleton(a), Singleton(b)) => if (a == b) true else false
    case (_, Bottom()) => true
    case (Bottom(), _) => false
    case _ => throw new IllegalArgumentException("")
  }

  def leastUpperBound(a: Elt, b: Elt): Elt = return (a, b) match {
    case (Top(), _) => Top()
    case (_, Top()) => Top()
    case (Singleton(a), Singleton(b)) => if (a == b) Singleton(a) else Top()
    case (Bottom(), _) => b
    case (_, Bottom()) => a
    case _ => throw new IllegalArgumentException("")
  }

  def greatestLowerBound(a: Elt, b: Elt): Elt = return (a, b) match {
    case (Top(), _) => b
    case (_, Top()) => a
    case (Singleton(a), Singleton(b)) => if (a == b) Singleton(a) else Bottom()
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case _ => throw new IllegalArgumentException("")
  }

  def eltToString(elt: FlatElt, indent: String) : String = elt match {
    case Top() => s"$indent Flat Top"
    case Bottom() => s"$indent Flat Bottom"
    case Singleton(a) => s"$indent Flat $a"
    case _ => throw new IllegalArgumentException("flatLattice error")
  }
}
