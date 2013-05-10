package tapy.lattices

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import java.math.BigInteger
import tapy.dfa._

sealed trait LongElt

object LongLattice extends Lattice[LongElt] {
  type Elt = LongElt
  
  case class Concrete(l:BigInteger) extends Elt
  case class Bottom() extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()

  def compare(op: cmpopType, e1: Elt, e2: Elt) : Option[Boolean] = (e1,e2) match {
    case (Concrete(s1), Concrete(s2)) => op match {
      case cmpopType.Eq => Some(s1.compareTo(s2) == 0)
      case cmpopType.NotEq => Some(s1.compareTo(s2) != 0)
      case cmpopType.Lt => Some(s1.compareTo(s2) < 0)
      case cmpopType.LtE => Some(s1.compareTo(s2) <= 0)
      case cmpopType.Gt => Some(s1.compareTo(s2) > 0)
      case cmpopType.GtE => Some(s1.compareTo(s2) >= 0)
      case _ => None
    }
    case _ => None
  }
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i.equals(j))
    case _ => false
  }

  def leastUpperBound(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Bottom(), Bottom()) => Bottom()
    case (Concrete(i),Bottom()) => Concrete(i)
    case (Bottom(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Abstract()
    case (_, _) =>  Abstract()
  }

  def greatestLowerBound(a: Elt, b: Elt) = (a, b) match {
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case (Abstract(), Abstract()) => Abstract()
    case (Concrete(i),Abstract()) => Concrete(i)
    case (Abstract(),Concrete(i)) => Concrete(i)
    case (Concrete(i),Concrete(j)) => if (i.equals(j)) Concrete(j) else Bottom()
    case (_, _) =>  Bottom()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent[Long $l]\n"
    case Bottom() => s"$indent[Not a Long (Bottom)]\n"
    case Abstract() => s"$indent[Any long (Top)]\n"
    case _ => throw new IllegalArgumentException("LongLattice pattern match error")
  }
  
  /* Element utility functions */
  
  def binaryOperator(el1: LongLattice.Elt, el2: LongLattice.Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(l1), Concrete(l2)) =>
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setLong(ValueLattice.bottom, l1.add(l2))
          case operatorType.Sub => ValueLattice.setLong(ValueLattice.bottom, l1.subtract(l2))
          case operatorType.Mult => ValueLattice.setLong(ValueLattice.bottom, l1.multiply(l2))
          case operatorType.Div => ValueLattice.setLong(ValueLattice.bottom, l1.divide(l2))
          case operatorType.Mod => ValueLattice.setLong(ValueLattice.bottom, l1.mod(l2))
          case operatorType.Pow => ValueLattice.setLong(ValueLattice.bottom, l1.pow(l2.intValue()))
          case operatorType.LShift => ValueLattice.setLong(ValueLattice.bottom, l1.shiftLeft(l2.intValue()))
          case operatorType.RShift => ValueLattice.setLong(ValueLattice.bottom, l1.shiftRight(l2.intValue()))
          case operatorType.BitOr => ValueLattice.setLong(ValueLattice.bottom, l1.or(l2))
          case operatorType.BitXor => ValueLattice.setLong(ValueLattice.bottom, l1.xor(l2))
          case operatorType.BitAnd => ValueLattice.setLong(ValueLattice.bottom, l1.and(l2))
          case operatorType.FloorDiv => throw new NotImplementedException()
        }
        
      case _ =>
        throw new NotImplementedException()
    }
  }
  
  def elementToFloat(el: LongLattice.Elt): FloatLattice.Elt = el match {
    case Abstract() => FloatLattice.Abstract()
    case Concrete(i) => FloatLattice.Concrete(i.floatValue())
    case Bottom() => FloatLattice.Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def elementToComplex(el: LongLattice.Elt): ComplexLattice.Elt = el match {
    case Abstract() => (FloatLattice.Abstract(), FloatLattice.Concrete(0))
    case Concrete(i) => (FloatLattice.Concrete(i.floatValue()), FloatLattice.Concrete(0))
    case Bottom() => (FloatLattice.Bottom(), FloatLattice.Bottom())
    case _ => throw new IllegalArgumentException()
  }
}