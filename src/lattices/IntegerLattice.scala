package tapy.lattices

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.unaryopType
import tapy.dfa._
import tapy.exceptions._

/*  Assuming that the python program is running on a 32 bit computer.
    This limits integers in python to -2**31-1 <= x <= 2**32-1. */

sealed trait IntegerElt

object IntegerLattice extends Lattice[IntegerElt] {
  type Elt = IntegerElt
  
  case class Concrete(i: Int) extends Elt {
    override def toString() = i.toString()
  }
  case class Bottom() extends Elt {
    override def toString() = ""
  }
  case class Abstract() extends Elt {
    override def toString() = "integer"
  }
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i == j)
    case _ => throw new IllegalArgumentException()
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Bottom(), Bottom()) =>  Bottom()
    case (Concrete(i),Bottom()) => Concrete(i)
    case (Bottom(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Abstract()
    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case (Abstract(), Abstract()) =>  Abstract()
    case (Concrete(i),Abstract()) => Concrete(i)
    case (Abstract(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Bottom()
    case (_, _) =>  Bottom()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent[Integer $l]\n"
    case Bottom() => s"$indent[Not an Integer (Bottom)]\n"
    case Abstract() => s"$indent[Any Integer (Top)]\n"
    case _ => throw new IllegalArgumentException("IntegerLattice pattern match error")
  }
  
  /* Element utility functions */

  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = (e1,e2) match {
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
  
  def binaryOperator(el1: IntegerLattice.Elt, el2: IntegerLattice.Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(i1), Concrete(i2)) =>
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

  def unaryOperator(el: Elt, op: unaryopType) : ValueLattice.Elt = el match {
    case (Concrete(i)) => op match {
      case unaryopType.Invert => ValueLattice.setInteger(~i)
      case unaryopType.Not => ValueLattice.setBoolean(i == 0)
      case unaryopType.UAdd => ValueLattice.setInteger(i)
      case unaryopType.USub => ValueLattice.setInteger(-i)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => op match {
      case unaryopType.Invert | unaryopType.UAdd | unaryopType.USub => ValueLattice.setIntegerElt(top)
      case unaryopType.Not => ValueLattice.setBooleanElt(BooleanLattice.top)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
  }
  
  def elementToFloat(el: IntegerLattice.Elt): FloatLattice.Elt = el match {
    case Abstract() => FloatLattice.Abstract()
    case Concrete(i) => FloatLattice.Concrete(i)
    case Bottom() => FloatLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToLong(el: IntegerLattice.Elt): LongLattice.Elt = el match {
    case Abstract() => LongLattice.Abstract()
    case Bottom() => LongLattice.Bottom()
    case Concrete(i) => LongLattice.Concrete(java.math.BigInteger.valueOf(i))
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToComplex(el: IntegerLattice.Elt): ComplexLattice.Elt = el match {
    case Abstract() => (FloatLattice.Abstract(), FloatLattice.Concrete(0))
    case Concrete(i) => (FloatLattice.Concrete(i), FloatLattice.Concrete(0))
    case Bottom() => (FloatLattice.Bottom(), FloatLattice.Bottom())
    case _ => throw new IllegalArgumentException()
  }
}
