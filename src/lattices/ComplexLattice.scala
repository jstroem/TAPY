package tapy.lattices

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import tapy.dfa._

object ComplexLattice extends ProductLattice(FloatLattice, FloatLattice) {
  
  /* Element utility functions */
  def elementCompare(op: cmpopType, e1: Elt, e2: Elt) : BooleanLattice.Elt = {
    val ((f1l,f1r),(f2l,f2r)) = (e1,e2)
    val cmpl = FloatLattice.elementCompare(op, f1l, f2l)
    val cmpr = FloatLattice.elementCompare(op, f1r, f2r)
    (cmpl,cmpr) match {
      case (BooleanLattice.Concrete(left),BooleanLattice.Concrete(right)) => op match {
        case cmpopType.Eq => BooleanLattice.Concrete(left && right)
        case cmpopType.NotEq => BooleanLattice.Concrete(left || right)
        case _ => BooleanLattice.top
      }
      case _ => BooleanLattice.top
    }
  }
  
  def binaryOperator(el1: Elt, el2: Elt, op: operatorType): ValueLattice.Elt = {
    (el1, el2) match {
      case ((FloatLattice.Concrete(r1), FloatLattice.Concrete(i1)), ((FloatLattice.Concrete(r2), FloatLattice.Concrete(i2)))) =>
        op match {
          case operatorType.UNDEFINED => throw new NotImplementedException()
          case operatorType.Add => ValueLattice.setComplex(ValueLattice.bottom, r1 + r2, i1 + i2)
          case operatorType.Sub => ValueLattice.setComplex(ValueLattice.bottom, r1 - r2, i1 - i2)
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