package tapy.lattices

import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import tapy.dfa._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

sealed trait BooleanElt

object BooleanLattice extends Lattice[BooleanElt] {
  type Elt = BooleanElt
  
  case class Concrete(b: Boolean) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()

  def make(ob: Option[Boolean]) : Elt = ob match {
    case None => top
    case Some(b) => Concrete(b)
  }

  def compare(op: cmpopType, a: Elt, b: Elt) : Option[Boolean] = (a,b) match {
    case (Concrete(s1), Concrete(s2)) => op match {
      case cmpopType.Eq => Some(s1 == s2)
      case cmpopType.NotEq => Some(s1 != s2)
      case cmpopType.Lt => Some(s1 < s2)
      case cmpopType.LtE => Some(s1 <= s2)
      case cmpopType.Gt => Some(s1 > s2)
      case cmpopType.GtE => Some(s1 >= s2)
      case _ => None
    }
    case _ => None
  }
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(true), Concrete(true)) => true
    case (Concrete(false), Concrete(false)) => true
    case _ => false
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Concrete(true), Concrete(true)) => Concrete(true)
    case (Concrete(false), Concrete(false)) => Concrete(false)
    case (Concrete(false), Bottom()) => Concrete(false)
    case (Concrete(true), Bottom()) => Concrete(true)
    case (Bottom(), Concrete(false)) => Concrete(false)
    case (Bottom(), Concrete(true)) => Concrete(true)
    case (Bottom(), Bottom()) => Bottom()
    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case (Concrete(true), Concrete(true)) => Concrete(true)
    case (Concrete(false), Concrete(false)) => Concrete(false)
    case (Concrete(false), Abstract()) => Concrete(false)
    case (Concrete(true), Abstract()) => Concrete(true)
    case (Abstract(), Concrete(false)) => Concrete(false)
    case (Abstract(), Concrete(true)) => Concrete(true)
    case (Abstract(), Abstract()) => Abstract()
    case (_, _) =>  Bottom()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent[Boolean: $l]\n"
    case Bottom() => s"$indent[Not a Boolean (Bottom)]\n"
    case Abstract() => s"$indent[Any String (Top)]\n"
    case _ => throw new IllegalArgumentException("BooleanLattice pattern match error")
  }
  
  /* Element utility functions */
  
  def binaryOperator(el1: BooleanLattice.Elt, el2: BooleanLattice.Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(b1), Concrete(b2)) =>
        val i1 = if (b1) 1 else 0
        val i2 = if (b2) 1 else 0
        
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setInteger(ValueLattice.bottom, i1 + i2)
          case operatorType.Sub => ValueLattice.setInteger(ValueLattice.bottom, i1 - i2)
          case operatorType.Mult => ValueLattice.setInteger(ValueLattice.bottom, i1 * i2)
          case operatorType.Div => ValueLattice.setInteger(ValueLattice.bottom, i1 / i2)
          case operatorType.Mod => ValueLattice.setInteger(ValueLattice.bottom, i1 % i2)
          case operatorType.Pow => ValueLattice.setInteger(ValueLattice.bottom, i1 ^ i2)
          case operatorType.LShift => throw new NotImplementedException()
          case operatorType.RShift => throw new NotImplementedException()
          case operatorType.BitOr => ValueLattice.setInteger(ValueLattice.bottom, i1 | i2)
          case operatorType.BitXor => throw new NotImplementedException()
          case operatorType.BitAnd => ValueLattice.setInteger(ValueLattice.bottom, i1 & i2)
          case operatorType.FloorDiv => throw new NotImplementedException()
        }
        
      case _ =>
        throw new NotImplementedException()
    }
  }
  
  def elementToInteger(el: BooleanLattice.Elt): IntegerLattice.Elt = el match {
    case Abstract() => IntegerLattice.Abstract()
    case Concrete(b) => IntegerLattice.Concrete(if (b) 1 else 0)
    case Bottom() => IntegerLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToFloat(el: BooleanLattice.Elt): FloatLattice.Elt = el match {
    case Abstract() => FloatLattice.Abstract()
    case Concrete(b) => FloatLattice.Concrete(if (b) 1 else 0)
    case Bottom() => FloatLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToLong(el: BooleanLattice.Elt): LongLattice.Elt = el match {
    case Abstract() => LongLattice.Abstract()
    case Concrete(b) => LongLattice.Concrete(java.math.BigInteger.valueOf(if (b) 1 else 0))
    case Bottom() => LongLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToComplex(el: BooleanLattice.Elt): ComplexLattice.Elt = el match {
    case Abstract() => (FloatLattice.Abstract(), FloatLattice.Concrete(0))
    case Concrete(b) => (FloatLattice.Concrete(if (b) 1 else 0), FloatLattice.Concrete(0))
    case Bottom() => (FloatLattice.Bottom(), FloatLattice.Bottom())
    case _ => throw new IllegalArgumentException()
  }
}
