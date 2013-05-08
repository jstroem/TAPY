package tapy.lattices

import tapy.dfa._

case class LatticeElt[T](lattice: Lattice[T], elt: T)