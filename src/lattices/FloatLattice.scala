package tapy.lattices

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.unaryopType
import org.python.antlr.ast.cmpopType
import java.lang.Double
import tapy.dfa._
import tapy.exceptions._

sealed trait FloatElt

object FloatLattice extends Lattice[FloatElt] {
  type Elt = FloatElt
  
  case class Concrete(f:Double) extends Elt {
    override def toString() = f.toString()
  }
  case class Bottom() extends Elt {
    override def toString() = "?"
  }
  case class Abstract() extends Elt {
    override def toString() = "float"
  }
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt) = (a, b) match {
    case (Abstract(), _)  => true
    case (_, Bottom()) => true
    case (Concrete(i),Concrete(j)) => (i.equals(j))
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
    case Concrete(l) => s"$indent[Float: $l]\n"
    case Bottom() => s"$indent[Not a Float (Bottom)]\n"
    case Abstract() => s"$indent[Any Float (Top)]\n"
    case _ => throw new IllegalArgumentException("FloatLattice pattern match error")
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

  def unaryOperator(el: Elt, op: unaryopType) : ValueLattice.Elt = el match {
    case (Concrete(f)) => op match {
      case unaryopType.Invert => throw new UnaryException("Float elements cannot do unaryop.Invert", op)
      case unaryopType.Not => ValueLattice.setBoolean(f == 0)
      case unaryopType.UAdd => ValueLattice.setFloat(f)
      case unaryopType.USub => ValueLattice.setFloat(-f)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => op match {
      case unaryopType.Invert => throw new UnaryException("Float elements cannot do unaryop.Invert", op)
      case unaryopType.UAdd | unaryopType.USub => ValueLattice.setFloatElt(top)
      case unaryopType.Not => ValueLattice.setBooleanElt(BooleanLattice.top)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
  }
  
  def binaryOperator(el1: Elt, el2: Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(f1), Concrete(f2)) =>
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setFloat(f1 + f2)
          case operatorType.Sub => ValueLattice.setFloat(f1 - f2)
          case operatorType.Mult => ValueLattice.setFloat(f1 * f2)
          case operatorType.Div => ValueLattice.setFloat(f1 / f2)
          case operatorType.Mod => ValueLattice.setFloat(f1 % f2)
          case operatorType.Pow => throw new NotImplementedException()
          case operatorType.LShift => throw new NotImplementedException()
          case operatorType.RShift => throw new NotImplementedException()
          case operatorType.BitOr => throw new NotImplementedException()
          case operatorType.BitXor => throw new NotImplementedException()
          case operatorType.BitAnd => throw new NotImplementedException()
          case operatorType.FloorDiv => throw new NotImplementedException()
        }
        
      case _ =>
        throw new NotImplementedException()
    }
  }
  
  def elementToComplex(el: Elt): ComplexLattice.Elt = el match {
    case Abstract() => (Abstract(), Concrete(0))
    case Concrete(i) => (Concrete(i), Concrete(0))
    case Bottom() => (Bottom(), Bottom())
    case _ => throw new IllegalArgumentException()
  }
}
