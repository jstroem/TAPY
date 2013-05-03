package tapy.lattices

import tapy.dfa._

class StackLattice(allocationSites: Set[Int], scopeChain: Set[Int], tempVars: Set[String])
extends ProductLattice(
    new MapLattice(new ValueLattice(allocationSites)),
    new ProductLattice(
        new FlatLattice[Int](),
        new FlatLattice[Int]()))