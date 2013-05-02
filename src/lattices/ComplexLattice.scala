package tapy.lattices

import tapy.dfa._

object ComplexLattice extends ProductLattice[FloatLattice.Elt, FloatLattice.Elt](FloatLattice, FloatLattice) { 
}