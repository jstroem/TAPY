package tapy.sign

import tapy.mfw._


// Elements of the set that are not present as keys in the Map are thought to be mapped to bottom element of lattice
class MapLattice[S, T](set: Set[S], lattice: Lattice[T]) extends Lattice[Map[S, T]] {

  def top: Map[S, T] = set.foldLeft (Map(): Map[S, T]) ((m, e) => m + (e -> lattice.top))
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
}
