package tapy.lattices

import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.python.antlr.ast.cmpopType

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
                ObjectLabelLattice)))))))) {
  
  /* Element utility functions */

  /** Used to guess the comparison result in a CompareNode given 2 valueElements. **/
  def elementCompare(op: cmpopType, left: Elt, right: Elt) : Elt = {
    if (elementIsUniqueConcreteString(left) && elementIsUniqueConcreteString(right))
      setBoolean(bottom, StringLattice.elementCompare(op, getString(left), getString(right)))
    else if (elementIsUniqueObjectLabel(left) && elementIsUniqueObjectLabel(right))
      setBoolean(bottom, ObjectLabelLattice.elementCompare(op, getObjectLabels(left), getObjectLabels(right)))
    else if (elementIsOnlyNone(left) && elementIsOnlyNone(right))
      setBoolean(bottom, NoneLattice.elementCompare(op, getNone(left), getNone(right)))
    else if (elementIsUniqueConcreteNumber(left) && elementIsUniqueConcreteNumber(right)) {
      val (leftCommon,rightCommon) = elementsToCommonType(left,right)
      if (elementIsOnlyInteger(leftCommon) && elementIsOnlyInteger(rightCommon))
        setBoolean(bottom, IntegerLattice.elementCompare(op, getInteger(leftCommon), getInteger(rightCommon)))
      else if (elementIsOnlyBoolean(leftCommon) && elementIsOnlyBoolean(rightCommon))
        setBoolean(bottom, BooleanLattice.elementCompare(op, getBoolean(leftCommon), getBoolean(rightCommon)))
      else if (elementIsOnlyFloat(leftCommon) && elementIsOnlyFloat(rightCommon))
        setBoolean(bottom, FloatLattice.elementCompare(op, getFloat(leftCommon), getFloat(rightCommon)))
      else if (elementIsOnlyLong(leftCommon) && elementIsOnlyLong(rightCommon)) 
        setBoolean(bottom, LongLattice.elementCompare(op, getLong(leftCommon), getLong(rightCommon)))
      else if (elementIsOnlyComplex(leftCommon) && elementIsOnlyComplex(rightCommon))
        setBoolean(bottom, ComplexLattice.elementCompare(op, getComplex(leftCommon), getComplex(rightCommon)))
      else 
        setBoolean(bottom, BooleanLattice.top)
    } else
      setBoolean(bottom, BooleanLattice.top)
  }
  
  def elementsToCommonType(el1: ValueLattice.Elt, el2: ValueLattice.Elt): (ValueLattice.Elt, ValueLattice.Elt) = {
    return (elementIsOnlyBoolean(el1), elementIsOnlyInteger(el1), elementIsOnlyFloat(el1), elementIsOnlyLong(el1), elementIsOnlyComplex(el1), 
            elementIsOnlyBoolean(el2), elementIsOnlyInteger(el2), elementIsOnlyFloat(el2), elementIsOnlyLong(el2), elementIsOnlyComplex(el2)) match {
      case (true, false, false, false, false, true, false, false, false, false) => (el1, el2) // boolean, boolean -> boolean, boolean
      case (true, false, false, false, false, false, true, false, false, false) => (setInteger(bottom, BooleanLattice.elementToInteger(getBoolean(el1))), el2) // boolean, integer -> integer, integer
      case (true, false, false, false, false, false, false, true, false, false) => (setFloat(bottom, BooleanLattice.elementToFloat(getBoolean(el1))), el2) // boolean, float -> float, float
      case (true, false, false, false, false, false, false, false, true, false) => (setLong(bottom, BooleanLattice.elementToLong(getBoolean(el1))), el2) // boolean, long -> long, long
      case (true, false, false, false, false, false, false, false, false, false) => (setComplex(bottom, BooleanLattice.elementToComplex(getBoolean(el1))), el2) // boolean, complex -> complex, complex
      
      case (false, true, false, false, false, true, false, false, false, false) => (el1, setInteger(bottom, BooleanLattice.elementToInteger(getBoolean(el2))))// integer, boolean -> integer, integer
      case (false, true, false, false, false, false, true, false, false, false) => (el1, el2) // integer, integer -> integer, integer
      case (false, true, false, false, false, false, false, true, false, false) => (setFloat(bottom, IntegerLattice.elementToFloat(getInteger(el1))), el2) // integer, float -> float, float
      case (false, true, false, false, false, false, false, false, true, false) => (setLong(bottom, IntegerLattice.elementToLong(getInteger(el1))), el2) // integer, long -> long, long
      case (false, true, false, false, false, false, false, false, false, true) => (setComplex(bottom, IntegerLattice.elementToComplex(getInteger(el1))), el2) // integer, complex -> complex, complex
      
      case (false, false, true, false, false, true, false, false, false, false) => (el1, setFloat(bottom, BooleanLattice.elementToFloat(getBoolean(el2)))) // float, boolean -> float, float
      case (false, false, true, false, false, false, true, false, false, false) => (el1, setFloat(bottom, IntegerLattice.elementToFloat(getInteger(el2)))) // float, integer -> float, float
      case (false, false, true, false, false, false, false, true, false, false) => (el1, el2) // float, float -> float, float
      case (false, false, true, false, false, false, false, false, true, false) => (el1, setFloat(bottom, LongLattice.elementToFloat(getLong(el2)))) // float, long -> float, float
      case (false, false, true, false, false, false, false, false, false, true) => (setComplex(bottom, FloatLattice.elementToComplex(getFloat(el1))), el2) // float, complex -> complex, complex
      
      case (false, false, false, true, false, true, false, false, false, false) => (el1, setLong(bottom, BooleanLattice.elementToLong(getBoolean(el2)))) // long, boolean -> long, long
      case (false, false, false, true, false, false, true, false, false, false) => (el1, setLong(bottom, IntegerLattice.elementToLong(getInteger(el2)))) // long, integer -> long, long
      case (false, false, false, true, false, false, false, true, false, false) => (setFloat(bottom, LongLattice.elementToFloat(getLong(el1))), el2) // long, float -> float, float
      case (false, false, false, true, false, false, false, false, true, false) => (el1, el2) // long, long -> long, long
      case (false, false, false, true, false, false, false, false, false, true) => (setComplex(bottom, LongLattice.elementToComplex(getLong(el1))), el2) // long, complex -> complex, complex
      
      case (false, false, false, false, true, true, false, false, false, false) => (el1, setComplex(bottom, BooleanLattice.elementToComplex(getBoolean(el2)))) // complex, boolean -> complex, complex
      case (false, false, false, false, true, false, true, false, false, false) => (el1, setComplex(bottom, IntegerLattice.elementToComplex(getInteger(el2)))) // complex, integer -> complex, complex
      case (false, false, false, false, true, false, false, true, false, false) => (el1, setComplex(bottom, FloatLattice.elementToComplex(getFloat(el2)))) // complex, float -> complex, complex
      case (false, false, false, false, true, false, false, false, true, false) => (el1, setComplex(bottom, LongLattice.elementToComplex(getLong(el2)))) // complex, long -> complex, complex
      case (false, false, false, false, true, false, false, false, false, true) => (el1, el2) // complex, complex -> complex, complex
      
      case _ => throw new NotImplementedException()
    }
  }

  /**
    * Element is only tests
    */
  
  def elementIsOnlyNone(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none != NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == Set())
  }
  
  def elementIsOnlyNumber(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    return (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && string == StringLattice.bottom && objectLabels == Set()) && (
        boolean != BooleanLattice.bottom || integer != IntegerLattice.bottom || float != FloatLattice.bottom || long != LongLattice.bottom || complex != ComplexLattice.bottom)
  }

  def elementIsOnlyBoolean(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean != BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == Set())
  }

  def elementIsOnlyInteger(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer != IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == Set())
  }

  def elementIsOnlyFloat(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float != FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == Set())
  }

  def elementIsOnlyLong(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long != LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == Set())
  }

  def elementIsOnlyComplex(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex != ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == Set())
  }

  def elementIsOnlyString(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string != StringLattice.bottom && objectLabels == Set())
  }

  /**
    * Element is unique concrete tests
    */
  
  def elementIsUniqueConcrete(el: ValueLattice.Elt): Boolean = {
    elementIsUniqueConcreteNumber(el) || elementIsUniqueConcreteString(el) || elementIsUniqueObjectLabel(el)
  }

  def elementIsUniqueConcreteNumber(el: ValueLattice.Elt): Boolean = {
    val (_, _, boolean, integer, float, long, complex, _, _) = ValueLattice.unpackElement(el)
    if (elementIsOnlyNumber(el))
      if (integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom) //boolean check
        boolean match {
          case BooleanLattice.Concrete(_) => true
          case _ => false
        }
      else if (boolean == BooleanLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom) //Integer check
        integer match {
          case IntegerLattice.Concrete(_) => true
          case _ => false
        }
      else if (boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom) //Float check
        float match {
          case FloatLattice.Concrete(_) => true
          case _ => false
        }
      else if (boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && complex == ComplexLattice.bottom) //Long check
        long match {
          case LongLattice.Concrete(_) => true
          case _ => false
        }
      else if (boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && long == LongLattice.bottom && float == FloatLattice.bottom) //Complex check
        complex match {
          case (FloatLattice.Concrete(_), FloatLattice.Concrete(_)) => true
          case _ => false
        }
      else
        false
    else
      false
  }

  def elementIsUniqueConcreteString(el: ValueLattice.Elt): Boolean = {
    if (elementIsOnlyString(el)){
      getString(el) match {
        case StringLattice.Concrete(_) => true
        case _ => false
      }
    } else {
      false
    }
  }

  def elementIsUniqueObjectLabel(el: ValueLattice.Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    return (undefined == UndefinedLattice.bottom &&
            none == NoneLattice.bottom &&
            boolean == BooleanLattice.bottom &&
            string == StringLattice.bottom &&
            objectLabels.size == 1 &&
            integer == IntegerLattice.bottom &&
            float == FloatLattice.bottom &&
            long == LongLattice.bottom &&
            complex == ComplexLattice.bottom)
  }

  /**
    * Least upper bound element
    */
  
  def lubElement(v: ValueLattice.Elt, elt: UndefinedLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setUndefined(v, UndefinedLattice.leastUpperBound(undefined, elt))
  }
  
  def lubElement(v: ValueLattice.Elt, elt: NoneLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setNone(v, NoneLattice.leastUpperBound(none, elt))
  }
  
  def lubElement(v: ValueLattice.Elt, elt: Boolean): ValueLattice.Elt = lubElement(v, BooleanLattice.Concrete(elt))
  def lubElement(v: ValueLattice.Elt, elt: BooleanLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setBoolean(v, BooleanLattice.leastUpperBound(boolean, elt))
  }
  
  def lubElement(v: ValueLattice.Elt, elt: Int): ValueLattice.Elt = lubElement(v, IntegerLattice.Concrete(elt))
  def lubElement(v: ValueLattice.Elt, elt: IntegerLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setInteger(v, IntegerLattice.leastUpperBound(integer, elt))
  }
  
  def lubElement(v: ValueLattice.Elt, elt: Double): ValueLattice.Elt = lubElement(v, FloatLattice.Concrete(elt))
  def lubElement(v: ValueLattice.Elt, elt: FloatLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setFloat(v, FloatLattice.leastUpperBound(float, elt))
  }
  
  def lubElement(v: ValueLattice.Elt, elt: java.math.BigInteger): ValueLattice.Elt = lubElement(v, LongLattice.Concrete(elt))
  def lubElement(v: ValueLattice.Elt, elt: LongLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setLong(v, LongLattice.leastUpperBound(long, elt))
  }
  
  def lubElement(v: ValueLattice.Elt, real: Double, imag: Double): ValueLattice.Elt = lubElement(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
  def lubElement(v: ValueLattice.Elt, elt: ComplexLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setComplex(v, ComplexLattice.leastUpperBound(complex, elt))
  }
  
  def lubElement(t: ValueLattice.Elt, el: String): ValueLattice.Elt = lubElement(t, StringLattice.Concrete(el))
  def lubElement(t: ValueLattice.Elt, el: StringLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(t)
    setString(t, StringLattice.leastUpperBound(string, el))
  }

  /**
    * Setters
    */
  
  def setUndefined(v: ValueLattice.Elt, undefined: UndefinedLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setNone(v: ValueLattice.Elt, none: NoneLattice.Elt): ValueLattice.Elt = {
    val (undefined, _, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v) 
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setBoolean(v: ValueLattice.Elt, elt: Boolean): ValueLattice.Elt = setBoolean(v, BooleanLattice.Concrete(elt))
  def setBoolean(v: ValueLattice.Elt, boolean: BooleanLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, _, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setInteger(v: ValueLattice.Elt, elt: Int): ValueLattice.Elt = setInteger(v, IntegerLattice.Concrete(elt))
  def setInteger(v: ValueLattice.Elt, integer: IntegerLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, _, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setFloat(v: ValueLattice.Elt, elt: Double): ValueLattice.Elt = setFloat(v, FloatLattice.Concrete(elt))
  def setFloat(v: ValueLattice.Elt, float: FloatLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, _, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setLong(v: ValueLattice.Elt, elt: java.math.BigInteger): ValueLattice.Elt = setLong(v, LongLattice.Concrete(elt))
  def setLong(v: ValueLattice.Elt, long: LongLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, _, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setComplex(v: ValueLattice.Elt, real: Double, imag: Double): ValueLattice.Elt = setComplex(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
  def setComplex(v: ValueLattice.Elt, complex: ComplexLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, _, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setString(v: ValueLattice.Elt, elt: String): ValueLattice.Elt = setString(v, StringLattice.Concrete(elt))
  def setString(v: ValueLattice.Elt, string: StringLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, _, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }

  def setObjectLabels(v: ValueLattice.Elt, objectLabels: ObjectLabelLattice.Elt): ValueLattice.Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, _) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }

  /* Getters */
  def getNone(v: ValueLattice.Elt) : NoneLattice.Elt = {
    val (_, none, _, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    none
  }

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
  
  def getObjectLabels(v: ValueLattice.Elt): ObjectLabelLattice.Elt = {
    val (_, _, _, _, _, _, _, _, objectLabels) = ValueLattice.unpackElement(v)
    objectLabels
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
                  objectLabels: ObjectLabelLattice.Elt = ObjectLabelLattice.bottom): ValueLattice.Elt = {
    return (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, objectLabels))))))))
  }

  def unpackElement(el: ValueLattice.Elt): (UndefinedLattice.Elt, NoneLattice.Elt, BooleanLattice.Elt, IntegerLattice.Elt, FloatLattice.Elt, LongLattice.Elt, ComplexLattice.Elt, StringLattice.Elt, ObjectLabelLattice.Elt) = {
    val (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, objectLabels)))))))) = el
    return (undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
}