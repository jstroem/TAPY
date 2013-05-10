package tapy.dfa

sealed trait MapElement

// Elements of the set that are not present as keys in the Map are thought to be mapped to bottom element of lattice
class MapLattice[S, T](lattice: Lattice[T]) extends Lattice[MapElement] {
  type Elt = MapElement

  case class Top() extends MapElement
  case class Concrete(m: Map[S, T]) extends MapElement

  def top: MapElement = Top()
  def bottom: MapElement = Concrete(Map(): Map[S, T])

  def compare(aa: MapElement, bb: MapElement) = (aa, bb) match {
    case (Concrete(a), Concrete(b)) => {
      val aKeys: Set[S] = a.keySet
      val bKeys: Set[S] = b.keySet

      (aKeys ++ bKeys).foldLeft (true) ((bigger: Boolean, key: S) => {
                                          val aVal: T = a.getOrElse(key, lattice.bottom)
                                          val bVal: T = b.getOrElse(key, lattice.bottom)
                                          (bigger && lattice.compare(aVal, bVal))
                                       })
    }
    case (Concrete(_), Top()) => false
    case _ => true
  }

  def leastUpperBound(aa: MapElement, bb: MapElement) = (aa, bb) match {
    case (Concrete(a), Concrete(b)) => {
      val aKeys: Set[S] = a.keySet
      val bKeys: Set[S] = b.keySet

      Concrete ((aKeys ++ bKeys).foldLeft (Map(): Map[S, T]) ((m: Map[S, T], key: S) => {
                                                      val aVal: T = a.getOrElse(key, lattice.bottom)
                                                      val bVal: T = b.getOrElse(key, lattice.bottom)
                                                      (m + (key -> lattice.leastUpperBound(aVal, bVal)))
                                                    }))
    }
    case _ => Top()
  }

  def greatestLowerBound(aa: MapElement, bb: MapElement) = (aa, bb) match {
    case (Concrete(a), Concrete(b)) => {
      val aKeys: Set[S] = a.keySet
      val bKeys: Set[S] = b.keySet

      Concrete ((aKeys ++ bKeys).foldLeft (Map(): Map[S, T]) ((m: Map[S, T], key: S) => {
                                                    val aVal: T = a.getOrElse(key, lattice.bottom)
                                                    val bVal: T = b.getOrElse(key, lattice.bottom)
                                                    (m + (key -> lattice.greatestLowerBound(aVal, bVal)))
                                                  }))
    }
    case (Top(), Concrete(a)) => Concrete(a)
    case (Concrete(a), Top()) => Concrete(a)
    case _ => Top()
  }

  def eltToString(e: MapElement, indent: String) : String = e match {
    case Concrete(elt) => {
      val keys: Set[S] = elt.keySet
        keys.foldLeft(indent+"Map\n") ((res: String, k: S) => {
          val key = k.toString
          val value = lattice.eltToString(elt(k), indent + "|   |   ")
          res +
            indent + "|   " + key + " =>\n" +
            value})
    }
    case _ => s"${indent}TopMap\n"
  }
  
  def get(map: MapElement, key: S): T = map match {
    case Concrete(m) => m.getOrElse(key, lattice.bottom)
    case _ => lattice.top
  }

  def update(e: MapElement, key: S, v: T): MapElement = e match {
    case Concrete(map) => Concrete(map + (key -> v))
    case _ => top
  }

}
