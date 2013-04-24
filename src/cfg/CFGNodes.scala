package tapy.cfg

import org.python.core._
import java.util.UUID
import tapy.constants

abstract class Node(id: UUID) {
  protected def reg(r: Int) = s"<$r>"
}

// Class and function declaration (exit and entry)
case class EntryNode(note: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = "EntryNode"
}
case class ExitNode(note: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = "ExitNode"
}

// Write variable; variable = value
// Write property into base (object/class); base.property = value
// Write property into dictionary: base[property] = value
case class WriteVariableNode(variable: String, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"$variable = ${reg(valueReg)}\n(WriteVariableNode)"
}
case class WriteRegisterNode(resultReg: Int, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(valueReg)}\n(WriteRegisterNode)"
}
case class WritePropertyNode(baseReg: Int, property: String, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(baseReg)}.$property = ${reg(valueReg)}\n(WritePropertyNode)"
}
case class WriteIndexableNode(baseReg: Int, propertyReg: Int, valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(baseReg)}[${reg(propertyReg)}] = ${reg(valueReg)}\n(WriteIndexableNode)"
}

//Constant expressions.
case class ConstantBooleanNode(resultReg: Int, bool: Boolean, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $bool\n(ConstantBooleanNode)"
}
case class ConstantIntNode(resultReg: Int, int: PyInteger, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $int\n(ConstantIntNode)"
}
case class ConstantFloatNode(resultReg: Int, float: PyFloat, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $float\n(ConstantFloatNode)"
}
case class ConstantLongNode(resultReg: Int, long: PyLong, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $long\n(ConstantLongNode)"
}
case class ConstantComplexNode(resultReg: Int, complex: PyComplex, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $complex\n(ConstantComplexNode)"
}
case class ConstantStringNode(resultReg: Int, string : String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $string\n(ConstantStringNode)"
}
case class ConstantNoneNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = None\n(ConstantNoneNode)"
}

case class NewListNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = []\n(NewListNode)"
}
case class NewDictionaryNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = {}\n(NewDictionaryNode)"
}
case class NewTupleNode(resultReg: Int, valueRegs: List[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString() = {
    val arg_string = valueRegs.foldLeft("") ({(s,r) => s+","+reg(r)})
    s"($arg_string)\n(NewTupleNode)"
  }
}
case class NewSetNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = {}\n(NewSetNode)"
}

// Read variable; result = variable
// Read property from base (object/class); result =  base.property
// Read property from dictionary: result = base[property]
case class ReadVariableNode(variable: String, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = $variable\n(ReadVariableNode)"
}
case class ReadPropertyNode(baseReg: Int, property: String, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(baseReg)}.$property\n(ReadPropertyNode)"
}
case class ReadIndexableNode(baseReg: Int, propertyReg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"${reg(resultReg)} = ${reg(baseReg)}.${reg(propertyReg)}\n(ReadIndexableNode)"
}

// Del
case class DelVariableNode(variable: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(DelVariableNode)"
}
case class DelIndexableNode(baseReg: Int, propertyReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(DelIndexableNode)"
}
case class DelPropertyNode(baseReg: Int, property: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(DelPropertyNode)"
}

// No Operation node; pass
case class NoOpNode(note: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = "$note\n(NoOpNode)"
}

// Break node
case class BreakNode(note: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = "$note\n(BreakNode)"
}

// If statement: if (condition): then_block else: else_block
case class IfNode(conditionReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = "if ${reg(conditionReg)}\n(IfNode)"
}

// Return statement: return result
case class ReturnNode(resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = "return ${reg(resultReg)}\n(ReturnNode)"
}

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(resultReg: Int, functionReg: Int, argument_regs: List[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString() = {
    val arg_string = argument_regs.foldLeft("") ({(s,r) => s+","+reg(r)})
    s"${reg(resultReg)} = ${reg(functionReg)}($arg_string)\n(CallNode)"
  }
}

// Raise error; Raise value
case class RaiseNode(valueReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"raise ${reg(valueReg)}\n(RaiseNode)"
}

// Except an exception; except [(]types[)] [as result]: 
case class ExceptNode(types: List[String], resultReg: Option[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(ExceptNode)"
}

// Binary operation; result = arg1 op arg2
case class BinOpNode(op: constants.BinOp.Value, arg1Reg: Int, arg2Reg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(BinOpNode)"
}
case class UnOpNode(op: constants.UnOp.Value, arg1Reg: Int, resultReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(UnOpNode)"
}

// Print node; print value
case class PrintNode(dest: Option[Int], valueRegs: List[Int], id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(Print)"
}

// For and while nodes
case class ForInNode(id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(ForInNode)"
}
case class WhileNode(condReg: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(WhileNode)"
}

// Misc
case class GlobalNode(variable: String, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"TODO\n(GlobalNode)"
}

case class AssertIterable(reg: Int, length: Int, id: UUID = UUID.randomUUID()) extends Node(id) {
  override def toString = s"assert(${reg(reg)}.length = $length)"
}
