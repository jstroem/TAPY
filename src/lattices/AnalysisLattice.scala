package tapy.lattices

import tapy.dfa._
import tapy.cfg._

object ProgramStateLattice extends MapLattice[Node, StateLattice.Elt](StateLattice)
object CallGraphLattice extends PowerSubSetLattice[(Any, Node, Any, Node)] // Any: Context sensitivity

object AnalysisLattice extends ProductLattice(
  ProgramStateLattice,
  CallGraphLattice)