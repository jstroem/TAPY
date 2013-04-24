package tapy.cfg

import org.python.core._
import java.util.UUID
import tapy.constants

abstract class Node(label: String, id: String) {
  protected def reg(r: Int) = s"<$r>"
}

// Class and function declaration (exit and entry)
case class EntryNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = "EntryNode"
}
case class ExitNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = "ExitNode"
}

// Write variable; variable = value
// Write property into base (object/class); base.property = value
// Write property into dictionary: base[property] = value
case class WriteVariableNode(variable: String, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"$variable = ${reg(valueReg)}\n(WriteVariableNode)"
}
case class WriteRegisterNode(resultReg: Int, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = ${reg(valueReg)}\n(WriteRegisterNode)"
}
case class WritePropertyNode(baseReg: Int, property: String, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(baseReg)}.$property = ${reg(valueReg)}\n(WritePropertyNode)"
}
case class WriteIndexableNode(baseReg: Int, propertyReg: Int, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(baseReg)}.${reg(propertyReg)} = ${reg(valueReg)}\n(WriteIndexableNode)"
}

//Constant expressions.
case class ConstantBooleanNode(resultReg: Int, bool: Boolean, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $bool\n(ConstantBooleanNode)"
}
case class ConstantIntNode(resultReg: Int, int: PyInteger, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $int\n(ConstantIntNode)"
}
case class ConstantFloatNode(resultReg: Int, float: PyFloat, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $float\n(ConstantFloatNode)"
}
case class ConstantLongNode(resultReg: Int, long: PyLong, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $long\n(ConstantLongNode)"
}
case class ConstantComplexNode(resultReg: Int, complex: PyComplex, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $complex\n(ConstantComplexNode)"
}
case class ConstantStringNode(resultReg: Int, string : String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $string\n(ConstantStringNode)"
}
case class ConstantNoneNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = None\n(ConstantNoneNode)"
}

case class NewListNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(NewListNode)"
}
case class NewDictionaryNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(NewDictionaryNode)"
}
case class NewTupleNode(resultReg: Int, valueRegs: List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(NewTupleNode)"
}
case class NewSetNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(NewSetNode)"
}

// Read variable; result = variable
// Read property from base (object/class); result =  base.property
// Read property from dictionary: result = base[property]
case class ReadVariableNode(variable: String, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = $variable\n(ReadVariableNode)"
}
case class ReadPropertyNode(baseReg: Int, property: String, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = ${reg(baseReg)}.$property\n(ReadPropertyNode)"
}
case class ReadIndexableNode(baseReg: Int, propertyReg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(resultReg)} = ${reg(baseReg)}.${reg(propertyReg)}\n(ReadIndexableNode)"
}

// Del
case class DelVariableNode(variable: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(DelVariableNode)"
}
case class DelIndexableNode(baseReg: Int, propertyReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(DelIndexableNode)"
}
case class DelPropertyNode(baseReg: Int, property: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(DelPropertyNode)"
}

// No Operation node; pass
case class NoOpNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = "$label\n(NoOpNode)"
}

// Break node
case class BreakNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = "break\n(BreakNode)"
}

// If statement: if (condition): then_block else: else_block
case class IfNode(conditionReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = "if ${reg(conditionReg)}\n(IfNode)"
}

// Return statement: return result
case class ReturnNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = "return ${reg(resultReg)}\n(ReturnNode)"
}

// Exceptional Return; 
case class ExceptionalReturnNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(ExceptionalReturnNode)"
}

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(resultReg: Int, functionReg: Int, argument_regs: List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"${reg(functionReg)}...TODO\n(CallNode)"
}

// Raise error; Raise value
case class RaiseNode(valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(RaiseNode)"
}

// Except an exception; except [(]types[)] [as result]: 
case class ExceptNode(types: List[String], resultReg: Option[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(ExceptNode)"
}

// Binary operation; result = arg1 op arg2
case class BinOpNode(op: constants.BinOp.Value, arg1Reg: Int, arg2Reg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(BinOpNode)"
}
case class UnOpNode(op: constants.UnOp.Value, arg1Reg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(UnOpNode)"
}

// Print node; print value
case class PrintNode(dest: Option[Int], valueRegs: List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// For and while nodes
case class ForInNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(ForInNode)"
}
case class WhileNode(condReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(WhileNode)"
}

// Misc
case class GlobalNode(variable: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"TODO\n(GlobalNode)"
}

case class AssertIterable(reg: Int, length: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id) {
  override def toString = s"assert(${reg(reg)}.length = $length)"
}
