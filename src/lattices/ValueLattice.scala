package tapy.lattices

import tapy.dfa._

object AllocationSiteLattice extends PowerSubSetLattice[String]

object ValueLattice
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
                AllocationSiteLattice)))))))) {

    def unpackElement(el: ValueLattice.Elt): (UndefinedLattice.Elt, NoneLattice.Elt, BooleanLattice.Elt, IntegerLattice.Elt, FloatLattice.Elt, LongLattice.Elt, ComplexLattice.Elt, StringLattice.Elt, AllocationSiteLattice.Elt) = {
      val (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, allocationSet)))))))) = el
      return (undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    /* Least upper bound elemt */
    
    def lubElement(v: ValueLattice.Elt, elt: UndefinedLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, UndefinedLattice.leastUpperBound(undefined, elt))
    }
    
    def lubElement(v: ValueLattice.Elt, elt: NoneLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, NoneLattice.leastUpperBound(none, elt))
    }
    
    def lubElement(v: ValueLattice.Elt, elt: Boolean): ValueLattice.Elt = lubElement(v, BooleanLattice.Concrete(elt))
    def lubElement(v: ValueLattice.Elt, elt: BooleanLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, BooleanLattice.leastUpperBound(boolean, elt))
    }
    
    def lubElement(v: ValueLattice.Elt, elt: Int): ValueLattice.Elt = lubElement(v, IntegerLattice.Concrete(elt))
    def lubElement(v: ValueLattice.Elt, elt: IntegerLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, IntegerLattice.leastUpperBound(integer, elt))
    }
    
    def lubElement(v: ValueLattice.Elt, elt: Double): ValueLattice.Elt = lubElement(v, FloatLattice.Concrete(elt))
    def lubElement(v: ValueLattice.Elt, elt: FloatLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, FloatLattice.leastUpperBound(float, elt))
    }
    
    def lubElement(v: ValueLattice.Elt, elt: java.math.BigInteger): ValueLattice.Elt = lubElement(v, LongLattice.Concrete(elt))
    def lubElement(v: ValueLattice.Elt, elt: LongLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, LongLattice.leastUpperBound(long, elt))
    }
    
    def lubElement(v: ValueLattice.Elt, real: Double, imag: Double): ValueLattice.Elt = lubElement(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
    def lubElement(v: ValueLattice.Elt, elt: ComplexLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      putElement(v, ComplexLattice.leastUpperBound(complex, elt))
    }
    
    def lubElement(t: ValueLattice.Elt, el: String): ValueLattice.Elt = lubElement(t, StringLattice.Concrete(el))
    def lubElement(t: ValueLattice.Elt, el: StringLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(t)
      putElement(t, StringLattice.leastUpperBound(string, el))
    }

    /* Put element */
    
    def putElement(v: ValueLattice.Elt, undefined: UndefinedLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, none: NoneLattice.Elt): ValueLattice.Elt = {
      val (undefined, _, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v) 
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, elt: Boolean): ValueLattice.Elt = putElement(v, BooleanLattice.Concrete(elt))
    def putElement(v: ValueLattice.Elt, boolean: BooleanLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, _, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, elt: Int): ValueLattice.Elt = putElement(v, IntegerLattice.Concrete(elt))
    def putElement(v: ValueLattice.Elt, integer: IntegerLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, _, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, elt: Double): ValueLattice.Elt = putElement(v, FloatLattice.Concrete(elt))
    def putElement(v: ValueLattice.Elt, float: FloatLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, _, long, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, elt: java.math.BigInteger): ValueLattice.Elt = putElement(v, LongLattice.Concrete(elt))
    def putElement(v: ValueLattice.Elt, long: LongLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, _, complex, string, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, real: Double, imag: Double): ValueLattice.Elt = putElement(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
    def putElement(v: ValueLattice.Elt, complex: ComplexLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, _, string, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }
    
    def putElement(v: ValueLattice.Elt, elt: String): ValueLattice.Elt = putElement(v, StringLattice.Concrete(elt))
    def putElement(v: ValueLattice.Elt, string: StringLattice.Elt): ValueLattice.Elt = {
      val (undefined, none, boolean, integer, float, long, complex, _, allocationSet) = ValueLattice.unpackElement(v)
      ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, allocationSet)
    }

    def packElement(undefined: UndefinedLattice.Elt = UndefinedLattice.bottom, 
                    none: NoneLattice.Elt = NoneLattice.bottom, 
                    boolean: BooleanLattice.Elt = BooleanLattice.bottom, 
                    integer: IntegerLattice.Elt = IntegerLattice.bottom, 
                    float: FloatLattice.Elt = FloatLattice.bottom, 
                    long: LongLattice.Elt = LongLattice.bottom, 
                    complex: ComplexLattice.Elt = ComplexLattice.bottom, 
                    string: StringLattice.Elt = StringLattice.bottom, 
                    allocationSet: AllocationSiteLattice.Elt = AllocationSiteLattice.bottom): ValueLattice.Elt = {
      return (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, allocationSet))))))))
    }
  }