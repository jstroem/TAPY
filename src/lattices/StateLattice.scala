package tapy.lattices

import tapy.dfa._

class StateLattice(allocationSites: Set[Int], scopeChain: Set[Int], tempVars: Set[String])
extends ProductLattice(
    new MapLattice(new ObjectLattice(allocationSites, scopeChain)),
    new ProductLattice(
        new StackLattice(allocationSites, scopeChain, tempVars),
        new ProductLattice(
            new PowerSubSetLattice(allocationSites),
            new PowerSubSetLattice(allocationSites))))