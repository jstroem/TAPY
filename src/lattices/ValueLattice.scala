package tapy.lattices

import tapy.dfa._

// T: The type of allocation sites
class ValueLattice[L]
extends ProductLattice(
    new UndefinedLattice(),
    new ProductLattice(
        new NoneLattice(),
        new ProductLattice(
            new BooleanLattice(),
            new ProductLattice(
                new IntegerLattice(),
                new ProductLattice(
                    new FloatLattice(),
                    new ProductLattice(
                        new LongLattice(),
                        new ProductLattice(
                            new ComplexLattice(),
                            new ProductLattice(
                                new StringLattice(),
                                new PowerSubSetLattice[L]()))))))))