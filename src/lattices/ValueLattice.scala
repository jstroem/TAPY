package tapy.lattices

import tapy.dfa._

class ValueLattice(allocationSites: Set[Int])
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
                                new PowerSubSetLattice(allocationSites)))))))))