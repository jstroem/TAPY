package tapy.lattices

import tapy.dfa._

class ComplexLattice extends MergeLattice[IntegerLattice.Elt, IntegerLattice.Elt](new IntegerLattice(), new IntegerLattice()) { 
}