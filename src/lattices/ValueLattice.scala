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

  def toString(el: Elt) : String = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    s"(%s,%s,%s,%s,%s,%s,%s,%s,%s)".format(undefined, none, boolean, integer, float, long, ComplexLattice.toString(complex), string, ObjectLabelLattice.toString(objectLabels))
  }

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
  
  def elementsToCommonType(el1: Elt, el2: Elt): (Elt, Elt) = {
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
  
  def elementIsOnlyNone(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none != NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }
  
  def elementIsOnlyNumber(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
     (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom) && (
        boolean != BooleanLattice.bottom || integer != IntegerLattice.bottom || float != FloatLattice.bottom || long != LongLattice.bottom || complex != ComplexLattice.bottom)
  }

  def elementIsOnlyBoolean(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean != BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyInteger(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer != IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyFloat(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float != FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyLong(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long != LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyComplex(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex != ComplexLattice.bottom && string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyString(el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string != StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyObjectLabels[T](el: Elt): Boolean = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom &&
        float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && string == StringLattice.bottom && objectLabels != ObjectLabelLattice.bottom) &&
        (objectLabels.foldLeft(true) {(acc, objectLabel) => acc && objectLabel.isInstanceOf[T]})
  }

  /**
    * Element is unique concrete tests
    */
  
  def elementIsUniqueConcrete(el: Elt): Boolean = {
    elementIsUniqueConcreteNumber(el) || elementIsUniqueConcreteString(el) || elementIsUniqueObjectLabel(el)
  }

  def elementIsUniqueConcreteNumber(el: Elt): Boolean = {
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

  def elementIsUniqueConcreteString(el: Elt): Boolean = {
    if (elementIsOnlyString(el)){
      getString(el) match {
        case StringLattice.Concrete(_) => true
        case _ => false
      }
    } else {
      false
    }
  }

  def elementIsUniqueObjectLabel(el: Elt): Boolean = {
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
  
  def lubElement(v: Elt, elt: UndefinedLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setUndefined(v, UndefinedLattice.leastUpperBound(undefined, elt))
  }
  
  def lubElement(v: Elt, elt: NoneLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setNone(v, NoneLattice.leastUpperBound(none, elt))
  }
  
  def lubElement(v: Elt, elt: Boolean): Elt = lubElement(v, BooleanLattice.Concrete(elt))
  def lubElement(v: Elt, elt: BooleanLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setBoolean(v, BooleanLattice.leastUpperBound(boolean, elt))
  }
  
  def lubElement(v: Elt, elt: Int): Elt = lubElement(v, IntegerLattice.Concrete(elt))
  def lubElement(v: Elt, elt: IntegerLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setInteger(v, IntegerLattice.leastUpperBound(integer, elt))
  }
  
  def lubElement(v: Elt, elt: Double): Elt = lubElement(v, FloatLattice.Concrete(elt))
  def lubElement(v: Elt, elt: FloatLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setFloat(v, FloatLattice.leastUpperBound(float, elt))
  }
  
  def lubElement(v: Elt, elt: java.math.BigInteger): Elt = lubElement(v, LongLattice.Concrete(elt))
  def lubElement(v: Elt, elt: LongLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setLong(v, LongLattice.leastUpperBound(long, elt))
  }
  
  def lubElement(v: Elt, real: Double, imag: Double): Elt = lubElement(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
  def lubElement(v: Elt, elt: ComplexLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    setComplex(v, ComplexLattice.leastUpperBound(complex, elt))
  }
  
  def lubElement(t: Elt, el: String): Elt = lubElement(t, StringLattice.Concrete(el))
  def lubElement(t: Elt, el: StringLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(t)
    setString(t, StringLattice.leastUpperBound(string, el))
  }

  /**
    * Setters
    */
  
  def setUndefined(v: Elt, undefined: UndefinedLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setNone(v: Elt, none: NoneLattice.Elt): Elt = {
    val (undefined, _, boolean, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v) 
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setBoolean(v: Elt, elt: Boolean): Elt = setBoolean(v, BooleanLattice.Concrete(elt))
  def setBoolean(v: Elt, boolean: BooleanLattice.Elt): Elt = {
    val (undefined, none, _, integer, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setInteger(v: Elt, elt: Int): Elt = setInteger(v, IntegerLattice.Concrete(elt))
  def setInteger(v: Elt, integer: IntegerLattice.Elt): Elt = {
    val (undefined, none, boolean, _, float, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setFloat(v: Elt, elt: Double): Elt = setFloat(v, FloatLattice.Concrete(elt))
  def setFloat(v: Elt, float: FloatLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, _, long, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setLong(v: Elt, elt: java.math.BigInteger): Elt = setLong(v, LongLattice.Concrete(elt))
  def setLong(v: Elt, long: LongLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, _, complex, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setComplex(v: Elt, real: Double, imag: Double): Elt = setComplex(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
  def setComplex(v: Elt, complex: ComplexLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, _, string, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setString(v: Elt, elt: String): Elt = setString(v, StringLattice.Concrete(elt))
  def setString(v: Elt, string: StringLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, _, objectLabels) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }

  def setObjectLabels(v: Elt, objectLabels: ObjectLabelLattice.Elt): Elt = {
    val (undefined, none, boolean, integer, float, long, complex, string, _) = ValueLattice.unpackElement(v)
    ValueLattice.packElement(undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }

  /* Getters */
  def getNone(v: Elt) : NoneLattice.Elt = {
    val (_, none, _, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    none
  }

  def getBoolean(v: Elt): BooleanLattice.Elt = {
    val (_, _, boolean, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    boolean
  }

  def getInteger(v: Elt): IntegerLattice.Elt = {
    val (_, _, _, integer, _, _, _, _, _) = ValueLattice.unpackElement(v)
    integer
  }

  def getFloat(v: Elt): FloatLattice.Elt = {
    val (_, _, _, _, float, _, _, _, _) = ValueLattice.unpackElement(v)
    float
  }

  def getLong(v: Elt): LongLattice.Elt = {
    val (_, _, _, _, _, long, _, _, _) = ValueLattice.unpackElement(v)
    long
  }

  def getComplex(v: Elt): ComplexLattice.Elt = {
    val (_, _, _, _, _, _, complex, _, _) = ValueLattice.unpackElement(v)
    complex
  }

  def getString(v: Elt): StringLattice.Elt = {
    val (_, _, _, _, _, _, _, string, _) = ValueLattice.unpackElement(v)
    string
  }
  
  def getObjectLabels(v: Elt): ObjectLabelLattice.Elt = {
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
                  objectLabels: ObjectLabelLattice.Elt = ObjectLabelLattice.bottom): Elt = {
    return (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, objectLabels))))))))
  }

  def unpackElement(el: Elt): (UndefinedLattice.Elt, NoneLattice.Elt, BooleanLattice.Elt, IntegerLattice.Elt, FloatLattice.Elt, LongLattice.Elt, ComplexLattice.Elt, StringLattice.Elt, ObjectLabelLattice.Elt) = {
    val (undefined, (none, (boolean, (integer, (float, (long, (complex, (string, objectLabels)))))))) = el
    return (undefined, none, boolean, integer, float, long, complex, string, objectLabels)
  }
}
