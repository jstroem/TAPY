package tapy.lattices

import tapy.dfa._

class ComplexLattice extends ProductLattice[FloatElt, FloatElt](new FloatLattice(), new FloatLattice())