package tapy.lattices

import tapy.dfa._
import tapy.exceptions._
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.unaryopType
import sun.reflect.generics.reflectiveObjects.NotImplementedException

sealed trait StringElt

object StringLattice extends Lattice[StringElt] {
  type Elt = StringElt
  
  case class Bottom() extends Elt {
    override def toString() = ""
  }
  case class Concrete(s: String) extends Elt {
    override def toString() = "'%s'".format(s)
  }
  case class Abstract() extends Elt {
    override def toString() = "string"
  }
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()
  
  def compare(a: Elt, b: Elt): Boolean = return (a, b) match {
    case (Abstract(), _) => true
    case (Concrete(_), Abstract()) => false
    case (Concrete(s1), Concrete(s2)) => s1 == s2
    case (Concrete(_), Bottom()) => true
    case (Bottom(), Abstract()) => false
    case (Bottom(), Bottom()) => true
    case (Bottom(), Concrete(_)) => false
    case _ => throw new IllegalArgumentException()
  }
  
  def leastUpperBound(a: Elt, b: Elt): Elt = return (a, b) match {
    case (Abstract(), _) => Abstract()
    case (_, Abstract()) => Abstract()
    case (Concrete(s1), Concrete(s2)) => if (s1 == s2) a else Abstract()
    case (Concrete(_), Bottom()) => a
    case (Bottom(), Concrete(_)) => b
    case (Bottom(), Bottom()) => Bottom()
    case _ => throw new IllegalArgumentException()
  }
  
  def greatestLowerBound(a: Elt, b: Elt): Elt = return (a, b) match {
    case (Abstract(), _) => b
    case (_, Abstract()) => a
    case (Concrete(s1), Concrete(s2)) => if (s1 == s2) a else Bottom()
    case (Bottom(), _) => Bottom()
    case (_, Bottom()) => Bottom()
    case _ => throw new IllegalArgumentException()
  }

  def eltToString(elt: Elt, indent: String): String = elt match {
    case Concrete(l) => s"$indent[String: $l]\n"
    case Bottom() => s"$indent[Not a String (Bottom)]\n"
    case Abstract() => s"$indent[Any String (Top)]\n"
    case _ => throw new IllegalArgumentException("StringLattice pattern match error")
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
    case (Concrete(str)) => op match {
      case unaryopType.Invert => throw new UnaryException("String elements cannot do this unary op", op)
      case unaryopType.Not => ValueLattice.setBoolean(str == "")
      case unaryopType.UAdd => throw new UnaryException("String elements cannot do this unary op", op)
      case unaryopType.USub => throw new UnaryException("String elements cannot do this unary op", op)
      case _ => throw new InternalErrorException("unaryopType was undefined")
    }
    case _ => op match {
      case unaryopType.Not => ValueLattice.setBooleanElt(BooleanLattice.top)
      case unaryopType.UAdd | unaryopType.USub | unaryopType.Invert => throw new UnaryException("String elements cannot do unaryop.Invert", op)
      case _ => throw new InternalErrorException("unaryopType was undefined") 
    }
  }
  
  def binaryOperator(el1: StringLattice.Elt, el2: StringLattice.Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(s1), Concrete(s2)) =>
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setString(s1 + s2)
          case operatorType.Sub => throw new NotImplementedException()
          case operatorType.Mult => throw new NotImplementedException()
          case operatorType.Div => throw new NotImplementedException()
          case operatorType.Mod => throw new NotImplementedException()
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
}
