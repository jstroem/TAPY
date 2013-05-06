package tapy.lattices

import tapy.dfa._

class ObjectLattice[L]
extends ProductLattice(
    new MapLattice(
        new ProductLattice(
            new ValueLattice(),
            new ProductLattice(
                new AbsentLattice(),
                new ModifiedLattice()))),
    new PowerSubSetLattice[L]())