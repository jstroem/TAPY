package tapy.lattices

import tapy.dfa._

class ComplexLattice extends MergeLattice[FloatLattice.Elt, FloatLattice.Elt](new FloatLattice(), new FloatLattice()) { 
}