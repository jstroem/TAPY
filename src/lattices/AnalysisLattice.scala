package tapy.lattices

import tapy.dfa._
import tapy.cfg._

class AnalysisLattice[L, C, N]
extends ProductLattice[
    MapLattice[Node, StateLattice[L]],
    PowerSubSetLattice[(C, N, C, N)]] // Call graph