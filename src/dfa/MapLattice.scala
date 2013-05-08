package tapy.dfa

// Elements of the set that are not present as keys in the Map are thought to be mapped to bottom element of lattice
class MapLattice[S, T](lattice: Lattice[T]) extends Lattice[Map[S, T]] {
  type Elt = Map[S, T]

  def top: Map[S, T] = throw new UnsupportedOperationException("No top available")
  def bottom: Map[S, T] = (Map(): Map[S, T])

  def compare(a: Map[S, T], b: Map[S, T]) = {
    val aKeys: Set[S] = a.keySet
    val bKeys: Set[S] = b.keySet

    (aKeys ++ bKeys).foldLeft (true) ((bigger: Boolean, key: S) => {
                                        val aVal: T = a.getOrElse(key, lattice.bottom)
                                        val bVal: T = b.getOrElse(key, lattice.bottom)
                                        (bigger && lattice.compare(aVal, bVal))
                                      })
  }

  def leastUpperBound(a: Map[S, T], b: Map[S, T]) = {
    val aKeys: Set[S] = a.keySet
    val bKeys: Set[S] = b.keySet

    (aKeys ++ bKeys).foldLeft (Map(): Map[S, T]) ((m: Map[S, T], key: S) => {
                                                    val aVal: T = a.getOrElse(key, lattice.bottom)
                                                    val bVal: T = b.getOrElse(key, lattice.bottom)
                                                    (m + (key -> lattice.leastUpperBound(aVal, bVal)))
                                                  })
  }

  def greatestLowerBound(a: Map[S, T], b: Map[S, T]) = {
    val aKeys: Set[S] = a.keySet
    val bKeys: Set[S] = b.keySet

    (aKeys ++ bKeys).foldLeft (Map(): Map[S, T]) ((m: Map[S, T], key: S) => {
                                                    val aVal: T = a.getOrElse(key, lattice.bottom)
                                                    val bVal: T = b.getOrElse(key, lattice.bottom)
                                                    (m + (key -> lattice.greatestLowerBound(aVal, bVal)))
                                                  })
  }
  
  def get(map: Map[S, T], key: S): T = {
    return map.getOrElse(key, lattice.bottom)
  }
}