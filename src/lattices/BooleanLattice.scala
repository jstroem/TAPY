package tapy.lattices

import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.unaryopType
import tapy.dfa._
import tapy.exceptions._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

sealed trait BooleanElt

object BooleanLattice extends Lattice[BooleanElt] {
  type Elt = BooleanElt
  
  case class Concrete(b: Boolean) extends Elt {
    override def toString() = b.toString()
  }
  case class Bottom() extends Elt {
    override def toString() = ""
  }
  case class Abstract() extends Elt {
    override def toString() = "boolean"
  }
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
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

  def elementCompare(op: cmpopType, a: Elt, b: Elt) : Elt = (a,b) match {
    case (Concrete(s1), Concrete(s2)) => op match {
      case cmpopType.Eq => BooleanLattice.Concrete(s1 == s2)
      case cmpopType.NotEq => BooleanLattice.Concrete(s1 != s2)
      case cmpopType.Lt => BooleanLattice.Concrete(s1 < s2)
      case cmpopType.LtE => BooleanLattice.Concrete(s1 <= s2)
      case cmpopType.Gt => BooleanLattice.Concrete(s1 > s2)
      case cmpopType.GtE => BooleanLattice.Concrete(s1 >= s2)
      case _ => BooleanLattice.top
    }
    case _ => BooleanLattice.top
  }

  def unaryOperator(el: Elt, op: unaryopType) : ValueLattice.Elt = el match {
    case (Concrete(b)) => op match {
      case unaryopType.Invert => ValueLattice.setInteger(if (b) -2 else -1)
      case unaryopType.Not => ValueLattice.setBoolean(!b)
      case unaryopType.UAdd => ValueLattice.setInteger(if (b) 1 else 0)
      case unaryopType.USub => ValueLattice.setInteger(if (b) -1 else 0)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => op match {
      case unaryopType.Invert | unaryopType.UAdd | unaryopType.USub => ValueLattice.setIntegerElt(IntegerLattice.top)
      case unaryopType.Not => ValueLattice.setBooleanElt(BooleanLattice.top)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
  }
  
  def binaryOperator(el1: Elt, el2: Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(b1), Concrete(b2)) =>
        val i1 = if (b1) 1 else 0
        val i2 = if (b2) 1 else 0
        
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setInteger(i1 + i2)
          case operatorType.Sub => ValueLattice.setInteger(i1 - i2)
          case operatorType.Mult => ValueLattice.setInteger(i1 * i2)
          case operatorType.Div => ValueLattice.setInteger(i1 / i2)
          case operatorType.Mod => ValueLattice.setInteger(i1 % i2)
          case operatorType.Pow => ValueLattice.setInteger(i1 ^ i2)
          case operatorType.LShift => throw new NotImplementedException()
          case operatorType.RShift => throw new NotImplementedException()
          case operatorType.BitOr => ValueLattice.setInteger(i1 | i2)
          case operatorType.BitXor => throw new NotImplementedException()
          case operatorType.BitAnd => ValueLattice.setInteger(i1 & i2)
          case operatorType.FloorDiv => throw new NotImplementedException()
        }
        
      case _ =>
        throw new NotImplementedException()
    }
  }
  
  def elementToInteger(el: Elt): IntegerLattice.Elt = el match {
    case Abstract() => IntegerLattice.Abstract()
    case Concrete(b) => IntegerLattice.Concrete(if (b) 1 else 0)
    case Bottom() => IntegerLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToFloat(el: Elt): FloatLattice.Elt = el match {
    case Abstract() => FloatLattice.Abstract()
    case Concrete(b) => FloatLattice.Concrete(if (b) 1 else 0)
    case Bottom() => FloatLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToLong(el: Elt): LongLattice.Elt = el match {
    case Abstract() => LongLattice.Abstract()
    case Concrete(b) => LongLattice.Concrete(java.math.BigInteger.valueOf(if (b) 1 else 0))
    case Bottom() => LongLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToComplex(el: Elt): ComplexLattice.Elt = el match {
    case Abstract() => (FloatLattice.Abstract(), FloatLattice.Concrete(0))
    case Concrete(b) => (FloatLattice.Concrete(if (b) 1 else 0), FloatLattice.Concrete(0))
    case Bottom() => (FloatLattice.Bottom(), FloatLattice.Bottom())
    case _ => throw new IllegalArgumentException()
  }
}
