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
  
  /* Element utility functions */

  def elementsToCommonType(el1: ValueLattice.Elt, el2: ValueLattice.Elt): (ValueLattice.Elt, ValueLattice.Elt) = {
    val (undefined1, none1, boolean1, integer1, float1, long1, complex1, string1, allocationSet1) = ValueLattice.unpackElement(el1)
    val (undefined2, none2, boolean2, integer2, float2, long2, complex2, string2, allocationSet2) = ValueLattice.unpackElement(el2)
    
    (elementIsOnlyInteger(el1), elementIsOnlyFloat(el1), elementIsOnlyLong(el1), elementIsOnlyComplex(el1), 
     elementIsOnlyInteger(el2), elementIsOnlyFloat(el2), elementIsOnlyLong(el2), elementIsOnlyComplex(el2)) match {
      case (true, false, false, false, true, false, false, false) => return (el1, el2) // integer, integer
      case (true, false, false, false, false, true, false, false) => return (el1, el2) // integer, float
      case (true, false, false, false, false, false, true, false) => return (el1, el2) // integer, long
      case (true, false, false, false, false, false, false, true) => return (el1, el2) // integer, complex
      
      case (false, true, false, false, true, false, false, false) => return (el1, el2) // float, integer
      case (false, true, false, false, false, true, false, false) => return (el1, el2) // float, float
      case (false, true, false, false, false, false, true, false) => return (el1, el2) // float, long
      case (false, true, false, false, false, false, false, true) => return (el1, el2) // float, complex
      
      case (false, false, true, false, true, false, false, false) => return (el1, el2) // long, integer
      case (false, false, true, false, false, true, false, false) => return (el1, el2) // long, float
      case (false, false, true, false, false, false, true, false) => return (el1, el2) // long, long
      case (false, false, true, false, false, false, false, true) => return (el1, el2) // long, complex
      
      case (false, false, false, true, true, false, false, false) => return (el1, el2) // complex, integer
      case (false, false, false, true, false, true, false, false) => return (el1, el2) // complex, float
      case (false, false, false, true, false, false, true, false) => return (el1, el2) // complex, long
      case (false, false, false, true, false, false, false, true) => return (el1, el2) // complex, complex
    }
      
    return (ValueLattice.bottom, ValueLattice.bottom)
  }

  /* Element tests */

  def elementIsNumber(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return (
        undefined == UndefinedLattice.bottom &&
        none == NoneLattice.bottom &&
        string == StringLattice.bottom &&
        allocationSet == Set()
      ) && (
        boolean != BooleanLattice.bottom ||
        integer != IntegerLattice.bottom ||
        float != FloatLattice.bottom ||
        long != LongLattice.bottom ||
        complex != ComplexLattice.bottom)
  }

  def elementIsOnlyInteger(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return ( undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer != IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && allocationSet == Set())
  }

  def elementIsOnlyFloat(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return ( undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float != FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && allocationSet == Set())
  }

  def elementIsOnlyLong(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return ( undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long != LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && allocationSet == Set())
  }

  def elementIsOnlyComplex(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return ( undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex != ComplexLattice.bottom && string == StringLattice.bottom && allocationSet == Set())
  }

  def elementIsUnique(el: ValueLattice.Elt): Boolean = {
    elementIsUniqueNumber(el) || elementIsUniqueString(el) || elementIsUniqueBoolean(el)
  }

  def elementIsUniqueNumber(el: ValueLattice.Elt): Boolean = {
    val (_, _, boolean, integer, float, long, complex, _, _) = ValueLattice.unpackElement(el)
    if (elementIsNumber(el)){
      if (integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom){ //boolean check
        boolean match {
          case BooleanLattice.Concrete(_) => true
          case _ => false
        }
      if (boolean == BooleanLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom){ //Integer check
        integer match {
          case IntegerLattice.Concrete(_) => true
          case _ => false
        }
      } else if (boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom) { //Float check
        float match {
          case FloatLattice.Concrete(_) => true
          case _ => false
        }
      } else if (boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && complex == ComplexLattice.bottom) { //Long check
        long match {
          case LongLattice.Concrete(_) => true
          case _ => false
        }
      } else if (boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && complex == ComplexLattice.bottom) { //Long check
        complex match {
          case (FloatLattice.Concrete(_), FloatLattice.Concrete(_)) => true
          case _ => false
        }
      } else {
        false
      }
    } else {
      false 
    }
  }

  def elementIsString(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return (undefined == UndefinedLattice.bottom &&
            none == NoneLattice.bottom &&
            boolean == BooleanLattice.bottom &&
            string != StringLattice.bottom &&
            allocationSet == Set() &&
            integer == IntegerLattice.bottom &&
            float == FloatLattice.bottom &&
            long == LongLattice.bottom &&
            complex == ComplexLattice.bottom)
  }

  def elementIsUniqueString(el: ValueLattice.Elt): Boolean = {
    if (elementIsString(el)){
      getString(el) match {
        case StringLattice.Concrete(_) => true
        case _ => false
      }
    } else {
      false
    }
  }

  def elementIsBoolean(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return (undefined == UndefinedLattice.bottom &&
            none == NoneLattice.bottom &&
            boolean != BooleanLattice.bottom &&
            string == StringLattice.bottom &&
            allocationSet == Set() &&
            integer == IntegerLattice.bottom &&
            float == FloatLattice.bottom &&
            long == LongLattice.bottom &&
            complex == ComplexLattice.bottom)
  }

  def elementIsUniqueBoolean(el: ValueLattice.Elt): Boolean = {
    if (elementIsBoolean(el)){
      getBoolean(el) match {
        case BooleanLattice.Concrete(_) => true
        case _ => false
      }
    } else {
      false
    }
  }

  def elementIsNone(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return (undefined == UndefinedLattice.bottom &&
            none != NoneLattice.bottom &&
            boolean == BooleanLattice.bottom &&
            string == StringLattice.bottom &&
            allocationSet == Set() &&
            integer == IntegerLattice.bottom &&
            float == FloatLattice.bottom &&
            long == LongLattice.bottom &&
            complex == ComplexLattice.bottom)
  }

  def elementIsUniqueAllocation(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, allocationSet) = ValueLattice.unpackElement(el)
    return (undefined == UndefinedLattice.bottom &&
            none == NoneLattice.bottom &&
            boolean == BooleanLattice.bottom &&
            string == StringLattice.bottom &&
            allocationSet.size == 1 &&
            integer == IntegerLattice.bottom &&
            float == FloatLattice.bottom &&
            long == LongLattice.bottom &&
            complex == ComplexLattice.bottom)
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

  /* Getters */

  def getBoolean(v: ValueLattice.Elt): BooleanLattice.Elt = {
    val (_, _, boolean, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    boolean
  }

  def getInteger(v: ValueLattice.Elt): IntegerLattice.Elt = {
    val (_, _, _, integer, _, _, _, _, _) = ValueLattice.unpackElement(v)
    integer
  }

  def getFloat(v: ValueLattice.Elt): FloatLattice.Elt = {
    val (_, _, _, _, float, _, _, _, _) = ValueLattice.unpackElement(v)
    float
  }

  def getLong(v: ValueLattice.Elt): LongLattice.Elt = {
    val (_, _, _, _, _, long, _, _, _) = ValueLattice.unpackElement(v)
    long
  }

  def getComplex(v: ValueLattice.Elt): ComplexLattice.Elt = {
    val (_, _, _, _, _, _, complex, _, _) = ValueLattice.unpackElement(v)
    complex
  }

  def getString(v: ValueLattice.Elt): StringLattice.Elt = {
    val (_, _, _, _, _, _, _, string, _) = ValueLattice.unpackElement(v)
    string
  }
  
  def getAllocationSet(v: ValueLattice.Elt): AllocationSiteLattice.Elt = {
    val (_, _, _, _, _, _, _, _, allocationSet) = ValueLattice.unpackElement(v)
    allocationSet
  }
  
  /* Pack and unpack */

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

  def unpackElement(el: ValueLattice.Elt): (UndefinedLattice.Elt, NoneLattice.Elt, BooleanLattice.Elt, IntegerLattice.Elt, FloatLattice.Elt, LongLattice.Elt, ComplexLattice.Elt, StringLattice.Elt, AllocationSiteLattice.Elt) = {
    val (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, allocationSet)))))))) = el
    return (undefined, none, boolean, integer, float, long, complex, string, allocationSet)
  }
}