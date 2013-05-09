package tapy.typeanalysis

import java.lang.ArithmeticException
import org.python.antlr.ast.operatorType
import org.python.antlr.ast.cmpopType
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

class TypeAnalysis(cfg: ControlFlowGraph) extends Analysis[AnalysisLattice.Elt] {
  type Elt = AnalysisLattice.Elt
  
  def bottom = AnalysisLattice.bottom
  
  /* Analysis interface */
  
  def generateConstraint(node: Node): Constraint[Elt] = {
    return node match {
        case node: ModuleEntryNode => ((solution) => handleModuleEntry(node, join(node, solution)))
        
        case node: ConstantBooleanNode => ((solution) => handleConstantBoolean(node, join(node, solution)))
        case node: ConstantIntNode => ((solution) => handleConstantInt(node, join(node, solution)))
        case node: ConstantFloatNode => ((solution) => handleConstantFloat(node, join(node, solution)))
        case node: ConstantLongNode => ((solution) => handleConstantLong(node, join(node, solution)))
        case node: ConstantComplexNode => ((solution) => handleConstantComplex(node, join(node, solution)))
        case node: ConstantStringNode => ((solution) => handleConstantString(node, join(node, solution)))
        case node: ConstantNoneNode => ((solution) => handleConstantNone(node, join(node, solution)))
        
        case node: ReadVariableNode => ((solution) => handleReadVariableNode(node, join(node, solution)))
        case node: WriteVariableNode => ((solution) => handleWriteVariableNode(node, join(node, solution)))
        
        case node: CompareOpNode => ((solution) => handleCompareOpNode(node, join(node, solution)))
        case node: BinOpNode => ((solution) => handleBinOpNode(node, join(node, solution)))
        case node: UnOpNode => ((solution) => handleUnOpNode(node, join(node, solution)))
        
        case node => ((solution) => join(node, solution))
      }
  }
  
  def nodeDependencies(cfgNode: Node): Set[Node] = {
    return cfg.getSuccessors(cfgNode)
  }
  
  def join(node: Node, solution: Elt): Elt = {
    val state = cfg.getPredecessors(node).foldLeft(StateLattice.bottom)((acc, pred) => 
      StateLattice.leastUpperBound(acc, AnalysisLattice.getState(pred, solution)))
    AnalysisLattice.setState(solution, node, state)
  }

  /* Misc */
  
  def handleModuleEntry(node: ModuleEntryNode, solution: Elt): Elt = {
    /* Create the main module object */
    AnalysisLattice.setExecutionContext(
      AnalysisLattice.updateHeap(solution, node, "__main__", ObjectLattice.bottom),
      node,
      ExecutionContextLattice.makeElement(List(), "__main__"))
  }

  /*
   * Constants: For each constant we can make a a strong update of the register, because there is
   * a unique register for each register index.
   */
  
  def handleConstantBoolean(node: ConstantBooleanNode, solution: Elt): Elt = {
    val value = ValueLattice.setBoolean(ValueLattice.bottom, node.bool)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantInt(node: ConstantIntNode, solution: Elt): Elt = {
    val value = ValueLattice.setInteger(ValueLattice.bottom, node.int.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantFloat(node: ConstantFloatNode, solution: Elt): Elt = {
    val value = ValueLattice.setFloat(ValueLattice.bottom, node.float.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantLong(node: ConstantLongNode, solution: Elt): Elt = {
    val value = ValueLattice.setLong(ValueLattice.bottom, node.long.getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantComplex(node: ConstantComplexNode, solution: Elt): Elt = {
    val value = ValueLattice.setComplex(ValueLattice.bottom, node.complex.getReal().getValue(), node.complex.getImag().getValue())
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantString(node: ConstantStringNode, solution: Elt): Elt = {
    val value = ValueLattice.setString(ValueLattice.bottom, node.string)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }

  def handleConstantNone(node: ConstantNoneNode, solution: Elt): Elt = {
    val value = ValueLattice.setNone(ValueLattice.bottom, NoneLattice.top)
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  /* Variables */
  
  def handleReadVariableNode(node: ReadVariableNode, solution: Elt): Elt = {
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    val value = variableObjects.foldLeft(ValueLattice.bottom) {(acc, variableObjectLabel) =>
      val variableObject = HeapLattice.getHeapObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val value = ObjectPropertyLattice.getValue(ObjectLattice.getObjectProperty(variableObject, node.variable))
      ValueLattice.leastUpperBound(value, acc)
    }
    
    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  def handleWriteVariableNode(node: WriteVariableNode, solution: Elt): Elt = {
    val value: ValueLattice.Elt = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.valueReg)
    
    val variableObjects = AnalysisLattice.getVariableObjects(solution, node)
    variableObjects.foldLeft(solution) {(acc, variableObjectLabel) =>
      val currentVariableObject = HeapLattice.getHeapObject(AnalysisLattice.getHeap(node, solution), variableObjectLabel)
      val newVariableObject = ObjectLattice.updatePropertyValue(currentVariableObject, node.variable, value)
      AnalysisLattice.updateHeap(acc, node, variableObjectLabel, newVariableObject)
    }
  }
  
  /* Operators */
  
  def handleCompareOpNode(node: CompareOpNode, solution: Elt): Elt = {
    val left: ValueLattice.Elt = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg1Reg)
    val right: ValueLattice.Elt = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg2Reg)

    val value = node.op match {
      case cmpopType.UNDEFINED => throw new NotImplementedException()
      case cmpopType.Eq | cmpopType.NotEq | cmpopType.Lt | cmpopType.LtE | cmpopType.Gt | cmpopType.GtE => {
        def operator[T <% Ordered[T]](op: cmpopType, e1: T, e2: T) = op match {
          case cmpopType.Eq => e1 == e2
          case cmpopType.NotEq => e1 != e2
          case cmpopType.Lt => e1 < e2
          case cmpopType.LtE => e1 <= e2
          case cmpopType.Gt => e1 > e2
          case cmpopType.GtE => e1 >= e2
          case _ => throw new NotImplementedException()
        }

        if (ValueLattice.elementIsUniqueConcreteString(left) && ValueLattice.elementIsUniqueConcreteString(right)) {

          val leftStringElt = ValueLattice.getString(left)
          val rightStringElt = ValueLattice.getString(right)
          (leftStringElt,rightStringElt) match {
            case (StringLattice.Concrete(leftString),StringLattice.Concrete(rightString)) => 
              ValueLattice.setBoolean(ValueLattice.bottom, operator(node.op, leftString, rightString)) //Operator on strings
            case _ => 
              ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
          }

        } else if (ValueLattice.elementIsUniqueConcreteNumber(left) && ValueLattice.elementIsUniqueConcreteNumber(right)) {

          val (leftValueElt,rightValueElt) = ValueLattice.elementsToCommonType(left,right)

          if (ValueLattice.elementIsOnlyInteger(leftValueElt) && ValueLattice.elementIsOnlyInteger(rightValueElt)) {

            val leftIntegerElt = ValueLattice.getInteger(leftValueElt)
            val rightIntegerElt = ValueLattice.getInteger(rightValueElt)
            (leftIntegerElt,rightIntegerElt) match {
              case (IntegerLattice.Concrete(leftInteger),IntegerLattice.Concrete(rightInteger)) => 
                ValueLattice.setBoolean(ValueLattice.bottom, operator(node.op, leftInteger, rightInteger)) //Operator on Ints
              case _ => 
                ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
            }

          } else if (ValueLattice.elementIsOnlyBoolean(leftValueElt) && ValueLattice.elementIsOnlyBoolean(rightValueElt)) {

            val leftBooleanElt = ValueLattice.getBoolean(leftValueElt)
            val rightBooleanElt = ValueLattice.getBoolean(rightValueElt)
            (leftBooleanElt,rightBooleanElt) match {
              case (BooleanLattice.Concrete(leftBoolean),BooleanLattice.Concrete(rightBoolean)) => 
                ValueLattice.setBoolean(ValueLattice.bottom, operator(node.op, leftBoolean, rightBoolean)) //Operator on Booleans
              case _ => 
                ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
            }

          } else if (ValueLattice.elementIsOnlyFloat(leftValueElt) && ValueLattice.elementIsOnlyFloat(rightValueElt)) {

            val leftFloatElt = ValueLattice.getFloat(leftValueElt)
            val rightFloatElt = ValueLattice.getFloat(rightValueElt)
            (leftFloatElt,rightFloatElt) match {
              case (FloatLattice.Concrete(leftFloat),FloatLattice.Concrete(rightFloat)) => 
                ValueLattice.setBoolean(ValueLattice.bottom, operator(node.op, leftFloat, rightFloat)) //Operator on Floats
              case _ => 
                ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
            }

          } else if (ValueLattice.elementIsOnlyLong(leftValueElt) && ValueLattice.elementIsOnlyLong(rightValueElt)) {

            val leftLongElt = ValueLattice.getLong(leftValueElt)
            val rightLongElt = ValueLattice.getLong(rightValueElt)
            (leftLongElt,rightLongElt) match {
              case (LongLattice.Concrete(leftLong),LongLattice.Concrete(rightLong)) => 
                ValueLattice.setBoolean(ValueLattice.bottom, operator(node.op, leftLong, rightLong)) //Operator on Longs
              case _ => 
                ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
            }

          } else if (ValueLattice.elementIsOnlyComplex(leftValueElt) && ValueLattice.elementIsOnlyComplex(rightValueElt)) {

            //In Complex values there is no orderings
            if (node.op == cmpopType.Eq || node.op == cmpopType.NotEq){
              val (leftFloatElt1,leftFloatElt2) = ValueLattice.getComplex(leftValueElt)
              val (rightFloatElt1,rightFloatElt2) = ValueLattice.getComplex(rightValueElt)
              (leftFloatElt1,leftFloatElt2,rightFloatElt1,rightFloatElt2) match {
                case (FloatLattice.Concrete(leftFloat1),FloatLattice.Concrete(leftFloat2),FloatLattice.Concrete(rightFloat1),FloatLattice.Concrete(rightFloat2)) => 
                  ValueLattice.setBoolean(ValueLattice.bottom, operator(node.op, leftFloat1, rightFloat1) && operator(node.op, leftFloat2, rightFloat2))
                case _ =>
                  ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
              }
            } else 
                ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)

          } else
            ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)


        } else {

          //In != and == we can check if registers points to the same object
          if ((node.op == cmpopType.Eq || node.op == cmpopType.NotEq) && ValueLattice.elementIsUniqueAllocation(left) && ValueLattice.elementIsUniqueAllocation(right)) {
            val leftAllocationsElt = ValueLattice.getAllocationSet(left)
            val rightAllocationsElt = ValueLattice.getAllocationSet(right)

            ValueLattice.setBoolean(ValueLattice.bottom, if (node.op == cmpopType.Eq) leftAllocationsElt == rightAllocationsElt else leftAllocationsElt != rightAllocationsElt)

          } else
            ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
        }
      }
      case cmpopType.Is => ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
      case cmpopType.IsNot => ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
      case cmpopType.In => ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
      case cmpopType.NotIn => ValueLattice.setBoolean(ValueLattice.bottom, BooleanLattice.top)
    }

    AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
  }
  
  /**
    * If left is exactly one number (including boolean) and right is exactly one number, the result is computed.
    */
  def handleBinOpNode(node: BinOpNode, solution: Elt): Elt = {
    // See:
    // - http://docs.python.org/2/reference/expressions.html#binary-bitwise-operations
    // - http://docs.python.org/2/reference/expressions.html#binary-arithmetic-operations
    
    var value: ValueLattice.Elt = null
    var exception = null
    
    val el1 = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg1Reg)
    val el2 = StackFrameLattice.getRegisterValue(AnalysisLattice.getStackFrame(node, solution), node.arg2Reg)
    
    if (ValueLattice.elementIsNumber(el1) && ValueLattice.elementIsNumber(el2)) {
      val (el1Common, el2Common) = ValueLattice.elementsToCommonType(el1, el2)
      
      val (undefined1, none1, boolean1, integer1, float1, long1, complex1, string1, allocationSet1) = ValueLattice.unpackElement(el1Common)
      val (undefined2, none2, boolean2, integer2, float2, long2, complex2, string2, allocationSet2) = ValueLattice.unpackElement(el2Common)
      
      try {
        if (ValueLattice.elementIsOnlyBoolean(el1Common) && ValueLattice.elementIsOnlyBoolean(el2Common))
          value = BooleanLattice.binaryOperator(ValueLattice.getBoolean(el1Common), ValueLattice.getBoolean(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyInteger(el1Common) && ValueLattice.elementIsOnlyInteger(el2Common))
          value = IntegerLattice.binaryOperator(ValueLattice.getInteger(el1Common), ValueLattice.getInteger(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyFloat(el1Common) && ValueLattice.elementIsOnlyFloat(el2Common))
          value = FloatLattice.binaryOperator(ValueLattice.getFloat(el1Common), ValueLattice.getFloat(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyLong(el1Common) && ValueLattice.elementIsOnlyLong(el2Common))
          value = LongLattice.binaryOperator(ValueLattice.getLong(el1Common), ValueLattice.getLong(el2Common), node.op)
        else if (ValueLattice.elementIsOnlyComplex(el1Common) && ValueLattice.elementIsOnlyComplex(el2Common))
          value = ComplexLattice.binaryOperator(ValueLattice.getComplex(el1Common), ValueLattice.getComplex(el2Common), node.op)
        else
          throw new NotImplementedException()
      } catch {
        case e: ArithmeticException => // TODO: Division by zero
      }
    } else {
      // TODO: TypeError
    }
    
    if (value != null)
      AnalysisLattice.updateStackFrame(solution, node, node.resultReg, value)
    else
      solution
  }
  
  def handleUnOpNode(node: UnOpNode, solution: Elt): Elt = {
    solution
  }
}