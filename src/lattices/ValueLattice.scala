package tapy.lattices

import tapy.dfa._
import tapy.exceptions._
import org.python.antlr.ast.cmpopType
import tapy.cfg._
import tapy.typeanalysis._

object ValueLattice
extends ProductLattice(
  UndefinedLattice, 
  new ProductLattice(
    NoneLattice, 
    new ProductLattice(
      NotImplementedLattice, 
      new ProductLattice(
        EllipsisLattice, 
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
                    ObjectLabelLattice)))))))))) with Logger {
  
  def undefined = setUndefined()
  
  /* Element utility functions */

  def toString(el: Elt) : String =
    ASTPrettyPrinter.implodeStringList(unpackElement(el).productIterator.toList.map((value) =>
      value.toString() match {
        case "(,)" => ""
        case "Set()" => ""
        case str => str
      }), ", ", true)

  override def eltToString(elt: Elt, indent: String) : String =
    indent + toString(elt)
  
  /** Used to guess the comparison result in a CompareNode given 2 valueElements. **/
  def elementCompare(op: cmpopType, left: Elt, right: Elt) : Elt = {
    if (elementIsUniqueConcreteString(left) && elementIsUniqueConcreteString(right))
      setBooleanElt(StringLattice.elementCompare(op, getString(left), getString(right)))
    else if (elementIsUniqueObjectLabel(left) && elementIsUniqueObjectLabel(right))
      setBooleanElt(ObjectLabelLattice.elementCompare(op, getObjectLabels(left), getObjectLabels(right)))
    else if (elementIsOnlyNone(left) && elementIsOnlyNone(right))
      setBooleanElt(NoneLattice.elementCompare(op, getNone(left), getNone(right)))
    else if (elementIsOnlyNotImplemented(left) && elementIsOnlyNotImplemented(right))
      setBooleanElt(NotImplementedLattice.elementCompare(op, getNotImplemented(left), getNotImplemented(right)))
    else if (elementIsOnlyEllipsis(left) && elementIsOnlyEllipsis(right))
      setBooleanElt(EllipsisLattice.elementCompare(op, getEllipsis(left), getEllipsis(right)))
    else if (elementIsUniqueConcreteNumber(left) && elementIsUniqueConcreteNumber(right)) {
      val (leftCommon,rightCommon) = elementsToCommonType(left,right)
      if (elementIsOnlyInteger(leftCommon) && elementIsOnlyInteger(rightCommon))
        setBooleanElt(IntegerLattice.elementCompare(op, getInteger(leftCommon), getInteger(rightCommon)))
      else if (elementIsOnlyBoolean(leftCommon) && elementIsOnlyBoolean(rightCommon))
        setBooleanElt(BooleanLattice.elementCompare(op, getBoolean(leftCommon), getBoolean(rightCommon)))
      else if (elementIsOnlyFloat(leftCommon) && elementIsOnlyFloat(rightCommon))
        setBooleanElt(FloatLattice.elementCompare(op, getFloat(leftCommon), getFloat(rightCommon)))
      else if (elementIsOnlyLong(leftCommon) && elementIsOnlyLong(rightCommon)) 
        setBooleanElt(LongLattice.elementCompare(op, getLong(leftCommon), getLong(rightCommon)))
      else if (elementIsOnlyComplex(leftCommon) && elementIsOnlyComplex(rightCommon))
        setBooleanElt(ComplexLattice.elementCompare(op, getComplex(leftCommon), getComplex(rightCommon)))
      else 
        setBooleanElt(BooleanLattice.top)
    } else
      setBooleanElt(BooleanLattice.top)
  }
  
  def elementsToCommonType(el1: Elt, el2: Elt): (Elt, Elt) = {
    return (elementIsOnlyBoolean(el1), elementIsOnlyInteger(el1), elementIsOnlyFloat(el1), elementIsOnlyLong(el1), elementIsOnlyComplex(el1), 
            elementIsOnlyBoolean(el2), elementIsOnlyInteger(el2), elementIsOnlyFloat(el2), elementIsOnlyLong(el2), elementIsOnlyComplex(el2)) match {
      case (true, false, false, false, false, true, false, false, false, false) => (el1, el2) // boolean, boolean -> boolean, boolean
      case (true, false, false, false, false, false, true, false, false, false) => (setIntegerElt(BooleanLattice.elementToInteger(getBoolean(el1))), el2) // boolean, integer -> integer, integer
      case (true, false, false, false, false, false, false, true, false, false) => (setFloatElt(BooleanLattice.elementToFloat(getBoolean(el1))), el2) // boolean, float -> float, float
      case (true, false, false, false, false, false, false, false, true, false) => (setLongElt(BooleanLattice.elementToLong(getBoolean(el1))), el2) // boolean, long -> long, long
      case (true, false, false, false, false, false, false, false, false, false) => (setComplexElt(BooleanLattice.elementToComplex(getBoolean(el1))), el2) // boolean, complex -> complex, complex
      
      case (false, true, false, false, false, true, false, false, false, false) => (el1, setIntegerElt(BooleanLattice.elementToInteger(getBoolean(el2))))// integer, boolean -> integer, integer
      case (false, true, false, false, false, false, true, false, false, false) => (el1, el2) // integer, integer -> integer, integer
      case (false, true, false, false, false, false, false, true, false, false) => (setFloatElt(IntegerLattice.elementToFloat(getInteger(el1))), el2) // integer, float -> float, float
      case (false, true, false, false, false, false, false, false, true, false) => (setLongElt(IntegerLattice.elementToLong(getInteger(el1))), el2) // integer, long -> long, long
      case (false, true, false, false, false, false, false, false, false, true) => (setComplexElt(IntegerLattice.elementToComplex(getInteger(el1))), el2) // integer, complex -> complex, complex
      
      case (false, false, true, false, false, true, false, false, false, false) => (el1, setFloatElt(BooleanLattice.elementToFloat(getBoolean(el2)))) // float, boolean -> float, float
      case (false, false, true, false, false, false, true, false, false, false) => (el1, setFloatElt(IntegerLattice.elementToFloat(getInteger(el2)))) // float, integer -> float, float
      case (false, false, true, false, false, false, false, true, false, false) => (el1, el2) // float, float -> float, float
      case (false, false, true, false, false, false, false, false, true, false) => (el1, setFloatElt(LongLattice.elementToFloat(getLong(el2)))) // float, long -> float, float
      case (false, false, true, false, false, false, false, false, false, true) => (setComplexElt(FloatLattice.elementToComplex(getFloat(el1))), el2) // float, complex -> complex, complex
      
      case (false, false, false, true, false, true, false, false, false, false) => (el1, setLongElt(BooleanLattice.elementToLong(getBoolean(el2)))) // long, boolean -> long, long
      case (false, false, false, true, false, false, true, false, false, false) => (el1, setLongElt(IntegerLattice.elementToLong(getInteger(el2)))) // long, integer -> long, long
      case (false, false, false, true, false, false, false, true, false, false) => (setFloatElt(LongLattice.elementToFloat(getLong(el1))), el2) // long, float -> float, float
      case (false, false, false, true, false, false, false, false, true, false) => (el1, el2) // long, long -> long, long
      case (false, false, false, true, false, false, false, false, false, true) => (setComplexElt(LongLattice.elementToComplex(getLong(el1))), el2) // long, complex -> complex, complex
      
      case (false, false, false, false, true, true, false, false, false, false) => (el1, setComplexElt(BooleanLattice.elementToComplex(getBoolean(el2)))) // complex, boolean -> complex, complex
      case (false, false, false, false, true, false, true, false, false, false) => (el1, setComplexElt(IntegerLattice.elementToComplex(getInteger(el2)))) // complex, integer -> complex, complex
      case (false, false, false, false, true, false, false, true, false, false) => (el1, setComplexElt(FloatLattice.elementToComplex(getFloat(el2)))) // complex, float -> complex, complex
      case (false, false, false, false, true, false, false, false, true, false) => (el1, setComplexElt(LongLattice.elementToComplex(getLong(el2)))) // complex, long -> complex, complex
      case (false, false, false, false, true, false, false, false, false, true) => (el1, el2) // complex, complex -> complex, complex
      
      case _ => throw new NotImplementedException()
    }
  }
  
  def splitElement(el: Elt): Set[Elt] = {
    var result = Set[Elt]()
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    if (undefined != UndefinedLattice.bottom) result = result + setUndefined(undefined)
    if (none != NoneLattice.bottom) result = result + setNone(none)
    if (notImplemented != NotImplementedLattice.bottom) result = result + setNotImplemented(notImplemented)
    if (ellipsis != EllipsisLattice.bottom) result = result + setEllipsis(ellipsis)
    if (boolean != BooleanLattice.bottom) result = result + setBooleanElt(boolean)
    if (integer != IntegerLattice.bottom) result = result + setIntegerElt(integer)
    if (float != FloatLattice.bottom) result = result + setFloatElt(float)
    if (long != LongLattice.bottom) result = result + setLongElt(long)
    if (complex != ComplexLattice.bottom) result = result + setComplexElt(complex)
    if (string != StringLattice.bottom) result = result + setStringElt(string)
    if (objectLabels != ObjectLabelLattice.bottom) result = result + setObjectLabels(objectLabels)
    result
  }

  /**
    * Element is only tests
    */
  
  def elementMaybeUndefined(el: Elt): Boolean = {
    return getUndefined(el) == UndefinedLattice.top
  }
  
  def elementIsOnlyNone(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none != NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom &&
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyNotImplemented(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented != NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom &&
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyEllipsis(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis != EllipsisLattice.bottom &&
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }
  
  def elementIsOnlyNumber(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
     (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom && 
      string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom) && (
      boolean != BooleanLattice.bottom || integer != IntegerLattice.bottom || float != FloatLattice.bottom || long != LongLattice.bottom || complex != ComplexLattice.bottom)
  }

  def elementIsOnlyBoolean(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom &&
     boolean != BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyInteger(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom &&
     boolean == BooleanLattice.bottom && integer != IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom &&
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyFloat(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom &&
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float != FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyLong(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom && 
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long != LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyComplex(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom && 
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex != ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyString(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom && 
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string != StringLattice.bottom && objectLabels == ObjectLabelLattice.bottom)
  }

  def elementIsOnlyObjectLabels[T <: AnyRef: Manifest](el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom && 
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     string == StringLattice.bottom && objectLabels != ObjectLabelLattice.bottom) && (objectLabels.foldLeft(true) {(acc, objectLabel) => acc && manifest[T].erasure.isInstance(objectLabel)})
  }

  def elementIsSubsciptable(el: Elt): Boolean = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    (undefined == UndefinedLattice.bottom && none == NoneLattice.bottom && notImplemented == NotImplementedLattice.bottom && ellipsis == EllipsisLattice.bottom && 
     boolean == BooleanLattice.bottom && integer == IntegerLattice.bottom && float == FloatLattice.bottom && long == LongLattice.bottom && complex == ComplexLattice.bottom && 
     (string != StringLattice.bottom || objectLabels != ObjectLabelLattice.bottom))
  }

  def elementIsDefinatelyTruthValue(el: Elt, which: Boolean): Boolean = {
    splitElement(el).foldLeft(true) {(acc, el) =>
      val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
      
      if (elementIsOnlyBoolean(el))
        boolean match {
          case BooleanLattice.Concrete(b) => if ((b && which) || (!b && !which)) acc else false
          case _ => false
        }
      else if (elementIsOnlyInteger(el))
        integer match {
          case IntegerLattice.Concrete(i) => if ((i != 0 && which) || (i == 0 && !which)) acc else false
          case _ => false
        }
      else if (elementIsOnlyFloat(el))
        float match {
          case FloatLattice.Concrete(f) => if ((f != 0 && which) || (f == 0 && !which)) acc else false
          case _ => false
        }
      else if (elementIsOnlyLong(el))
        long match {
          case LongLattice.Concrete(l) => if ((l != 0 && which) || (l == 0 && !which)) acc else false
          case _ => false
        }
      else if (elementIsOnlyComplex(el))
        complex match {
          case (FloatLattice.Concrete(r), FloatLattice.Concrete(i)) => if (((r != 0 || i != 0) && which) || ((r == 0 && i == 0) && !which)) acc else false
          case _ => false
        }
      else if (elementIsOnlyString(el))
        string match {
          case StringLattice.Concrete(s) => if ((s != "" && which) || (s == "" && !which)) acc else false
          case _ => false
        }
      else
        false
    }
  }
  
  /**
    * Element is unique concrete tests
    */
  
  def elementIsUniqueConcrete(el: Elt): Boolean = {
    elementIsUniqueConcreteNumber(el) || elementIsUniqueConcreteString(el) || elementIsUniqueObjectLabel(el)
  }

  def elementIsUniqueConcreteNumber(el: Elt): Boolean = {
    val (_, _,_,_, boolean, integer, float, long, complex, _, _) = ValueLattice.unpackElement(el)
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
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    return (undefined == UndefinedLattice.bottom &&
            none == NoneLattice.bottom &&
            notImplemented == NotImplementedLattice.bottom &&
            ellipsis == EllipsisLattice.bottom &&
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
    setUndefined(UndefinedLattice.leastUpperBound(getUndefined(v), elt), v)
  }
  
  def lubElement(v: Elt, elt: NoneLattice.Elt): Elt = {
    setNone(NoneLattice.leastUpperBound(getNone(v), elt), v)
  }

  def lubElement(v: Elt, elt: NotImplementedLattice.Elt): Elt = {
    setNotImplemented(NotImplementedLattice.leastUpperBound(getNotImplemented(v), elt), v)
  }


  def lubElement(v: Elt, elt: EllipsisLattice.Elt): Elt = {
    setEllipsis(EllipsisLattice.leastUpperBound(getEllipsis(v), elt), v)
  }
  
  def lubElement(v: Elt, elt: Boolean): Elt = lubElement(v, BooleanLattice.Concrete(elt))
  def lubElement(v: Elt, elt: BooleanLattice.Elt): Elt = {
    setBooleanElt(BooleanLattice.leastUpperBound(getBoolean(v), elt), v)
  }
  
  def lubElement(v: Elt, elt: Int): Elt = lubElement(v, IntegerLattice.Concrete(elt))
  def lubElement(v: Elt, elt: IntegerLattice.Elt): Elt = {
    setIntegerElt(IntegerLattice.leastUpperBound(getInteger(v), elt), v)
  }
  
  def lubElement(v: Elt, elt: Double): Elt = lubElement(v, FloatLattice.Concrete(elt))
  def lubElement(v: Elt, elt: FloatLattice.Elt): Elt = {
    setFloatElt(FloatLattice.leastUpperBound(getFloat(v), elt), v)
  }
  
  def lubElement(v: Elt, elt: java.math.BigInteger): Elt = lubElement(v, LongLattice.Concrete(elt))
  def lubElement(v: Elt, elt: LongLattice.Elt): Elt = {
    setLongElt(LongLattice.leastUpperBound(getLong(v), elt))
  }
  
  def lubElement(v: Elt, real: Double, imag: Double): Elt = lubElement(v, (FloatLattice.Concrete(real), FloatLattice.Concrete(imag)))
  def lubElement(v: Elt, elt: ComplexLattice.Elt): Elt = {
    setComplexElt(ComplexLattice.leastUpperBound(getComplex(v), elt), v)
  }
  
  def lubElement(v: Elt, el: String): Elt = lubElement(v, StringLattice.Concrete(el))
  def lubElement(v: Elt, el: StringLattice.Elt): Elt = {
    setStringElt(StringLattice.leastUpperBound(getString(v), el), v)
  }

  /**
    * Setters
    */
  
  def setUndefined(undefined: UndefinedLattice.Elt = UndefinedLattice.top, el: Elt = bottom): Elt = {
    val (_, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setNone(none: NoneLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, _, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }

  def setNotImplemented(notImplemented: NotImplementedLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, _, ellipsis, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }

  def setEllipsis(ellipsis: EllipsisLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, _, boolean, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setBoolean(boolean: Boolean, el: Elt = bottom): Elt = setBooleanElt(BooleanLattice.Concrete(boolean), el)
  def setBooleanElt(boolean: BooleanLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, _, integer, float, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setInteger(integer: Int, el: Elt = bottom): Elt = setIntegerElt(IntegerLattice.Concrete(integer), el)
  def setIntegerElt(integer: IntegerLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, boolean, _, float, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setFloat(float: Double, el: Elt = bottom): Elt = setFloatElt(FloatLattice.Concrete(float), el)
  def setFloatElt(float: FloatLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, _, long, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setLong(long: java.math.BigInteger, el: Elt = bottom): Elt = setLongElt(LongLattice.Concrete(long), el)
  def setLongElt(long: LongLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, _, complex, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setComplex(real: Double, imag: Double, el: Elt = bottom): Elt = setComplexElt((FloatLattice.Concrete(real), FloatLattice.Concrete(imag)), el)
  def setComplexElt(complex: ComplexLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, _, string, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
  
  def setString(string: String, el: Elt = bottom): Elt = setStringElt(StringLattice.Concrete(string), el)
  def setStringElt(string: StringLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, _, objectLabels) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }

  def setObjectLabels(objectLabels: ObjectLabelLattice.Elt, el: Elt = bottom): Elt = {
    val (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, _) = unpackElement(el)
    ValueLattice.packElement(undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }

  /* Getters */
  
  def getUndefined(v: Elt) : UndefinedLattice.Elt = {
    val (undefined, _, _, _, _, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    undefined
  }

  def getNone(v: Elt) : NoneLattice.Elt = {
    val (_, none, _, _, _, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    none
  }

  def getNotImplemented(v: Elt) : NotImplementedLattice.Elt = {
    val (_, _, notImplemented, _, _, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    notImplemented
  }

  def getEllipsis(v: Elt) : EllipsisLattice.Elt = {
    val (_, _, _, ellipsis, _, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    ellipsis
  }

  def getBoolean(v: Elt): BooleanLattice.Elt = {
    val (_, _, _, _, boolean, _, _, _, _, _, _) = ValueLattice.unpackElement(v)
    boolean
  }

  def getInteger(v: Elt): IntegerLattice.Elt = {
    val (_, _, _, _, _, integer, _, _, _, _, _) = ValueLattice.unpackElement(v)
    integer
  }

  def getFloat(v: Elt): FloatLattice.Elt = {
    val (_, _, _, _, _, _, float, _, _, _, _) = ValueLattice.unpackElement(v)
    float
  }

  def getLong(v: Elt): LongLattice.Elt = {
    val (_, _, _, _, _, _, _, long, _, _, _) = ValueLattice.unpackElement(v)
    long
  }

  def getComplex(v: Elt): ComplexLattice.Elt = {
    val (_, _, _, _, _, _, _, _, complex, _, _) = ValueLattice.unpackElement(v)
    complex
  }

  def getString(v: Elt): StringLattice.Elt = {
    val (_, _, _, _, _, _, _, _, _, string, _) = ValueLattice.unpackElement(v)
    string
  }
  
  def getObjectLabels(v: Elt): ObjectLabelLattice.Elt = {
    val (_, _, _, _, _, _, _, _, _, _, objectLabels) = ValueLattice.unpackElement(v)
    objectLabels
  }
  
  /**
    * Throws an UnexpectedValueException if the value given is not exactly one object label.
    */
  def getSingleObjectLabel(v: Elt): ObjectLabel = {
    if (!elementIsOnlyObjectLabels[ObjectLabel](v)) {
      throw new UnexpectedValueException("Value not only object labels (actual: " + v + ")")
    }
    
    val labels = getObjectLabels(v)
    if (labels.size != 1) {
      throw new UnexpectedValueException("Value not exactly one object label (actual: " + v + ")")
    }
    
    return labels.head
  }
  
  /* Pack and unpack */

  def packElement(undefined: UndefinedLattice.Elt = UndefinedLattice.bottom, 
                  none: NoneLattice.Elt = NoneLattice.bottom, 
                  notImplemented: NotImplementedLattice.Elt = NotImplementedLattice.bottom, 
                  ellipsis: EllipsisLattice.Elt = EllipsisLattice.bottom, 
                  boolean: BooleanLattice.Elt = BooleanLattice.bottom, 
                  integer: IntegerLattice.Elt = IntegerLattice.bottom, 
                  float: FloatLattice.Elt = FloatLattice.bottom, 
                  long: LongLattice.Elt = LongLattice.bottom, 
                  complex: ComplexLattice.Elt = ComplexLattice.bottom, 
                  string: StringLattice.Elt = StringLattice.bottom, 
                  objectLabels: ObjectLabelLattice.Elt = ObjectLabelLattice.bottom): Elt = {
    return (undefined, (none, (notImplemented, (ellipsis, (boolean, (integer, (float, (long, (complex, (string, objectLabels))))))))))
  }

  def unpackElement(el: Elt): (UndefinedLattice.Elt, NoneLattice.Elt, NotImplementedLattice.Elt, EllipsisLattice.Elt, BooleanLattice.Elt, IntegerLattice.Elt, FloatLattice.Elt, LongLattice.Elt, ComplexLattice.Elt, StringLattice.Elt, ObjectLabelLattice.Elt) = {
    val (undefined, (none, (notImplemented, (ellipsis, (boolean, (integer, (float, (long, (complex, (string, objectLabels)))))))))) = el
    return (undefined, none, notImplemented, ellipsis, boolean, integer, float, long, complex, string, objectLabels)
  }
}