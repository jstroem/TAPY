package tapy.lattices

import tapy.dfa._

class AnalysisLattice[L, C, N, F]
extends ProductLattice(
    new MapLattice(new StateLattice[L]()),
    new PowerSubSetLattice[(C, N, C, F)]()) // Call graph