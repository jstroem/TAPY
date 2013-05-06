package tapy.lattices

import tapy.dfa._

class AnalysisLattice[L, C, N] extends ProductLattice(
  new MapLattice(new StateLattice[L]()),
  new PowerSubSetLattice[(C, N, C, N)]()) // Call graph