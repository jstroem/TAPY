package tapy.lattices

import tapy.dfa._
import org.python.antlr.ast.cmpopType
import org.python.antlr.ast.operatorType
import sun.reflect.generics.reflectiveObjects.NotImplementedException

sealed trait StringElt

object StringLattice extends Lattice[StringElt] {
  type Elt = StringElt
  
  case class Bottom() extends Elt
  case class Concrete(s: String) extends Elt
  case class Abstract() extends Elt
  
  def top: Elt = Abstract()
  def bottom: Elt = Bottom()

  def compare(op: cmpopType, e1: Elt, e2: Elt) : Option[Boolean] = (e1,e2) match {
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
  
  def binaryOperator(el1: StringLattice.Elt, el2: StringLattice.Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case (Concrete(s1), Concrete(s2)) =>
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setString(ValueLattice.bottom, s1 + s2)
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