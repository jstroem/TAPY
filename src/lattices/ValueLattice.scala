package tapy.lattices

import tapy.dfa._

// T: The type of allocation sites
class ValueLattice[L]
extends ProductLattice(
    UndefinedLattice,
    new ProductLattice(
        NoneLattice,
        new ProductLattice(
            BooleanLattice,
            new ProductLattice(
                IntegerLattice,
                new ProductLattice(
                    FloatLattice,
                    new ProductLattice(
                        LongLattice,
                        new ProductLattice(
                            ComplexLattice,
                            new ProductLattice(
                                StringLattice,
                                new PowerSubSetLattice[L]()))))))))