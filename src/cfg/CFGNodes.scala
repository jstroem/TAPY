package tapy.cfg

import org.python.core._
import java.util.UUID
import tapy.constants

abstract class Node(label: String, id: String)

// Class and function declaration (exit and entry)
case class EntryNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ExitNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Write variable; variable = value
// Write property into base (object/class); base.property = value
// Write property into dictionary: base[property] = value
case class WriteVariableNode(variable: String, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WriteRegisterNode(resultReg: Int, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WritePropertyNode(baseReg: Int, property: String, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WriteIndexableNode(baseReg: Int, propertyReg: Int, valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

case class ConstantBooleanNode(resultReg: Int, bool: Boolean, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantIntNode(resultReg: Int, int: PyInteger, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantFloatNode(resultReg: Int, float: PyFloat, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantLongNode(resultReg: Int, long: PyLong, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantComplexNode(resultReg: Int, complex: PyComplex, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantStringNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantNoneNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

case class NewListNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class NewDictionaryNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class NewTupleNode(resultReg: Int, valueRegs: List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class NewSetNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Read variable; result = variable
// Read property from base (object/class); result =  base.property
// Read property from dictionary: result = base[property]
case class ReadVariableNode(variable: String, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ReadPropertyNode(baseReg: Int, property: String, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ReadIndexableNode(baseReg: Int, propertyReg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Del
case class DelVariableNode(variable: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class DelIndexableNode(baseReg: Int, propertyReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class DelPropertyNode(baseReg: Int, property: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// No Operation node; pass
case class NoOpNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Break node
case class BreakNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// If statement: if (condition): then_block else: else_block
case class IfNode(conditionReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Return statement: return result
case class ReturnNode(resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Exceptional Return; 
case class ExceptionalReturnNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(resultReg: Int, functionReg: Int, argument_regs: List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Raise error; Raise value
case class RaiseNode(valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Except an exception; except [(]types[)] [as result]: 
case class ExceptNode(types: List[String], resultReg: Option[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Binary operation; result = arg1 op arg2
case class BinOpNode(op: constants.BinOp.Value, arg1Reg: Int, arg2Reg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class UnOpNode(op: constants.UnOp.Value, arg1Reg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Print node; print value
case class PrintNode(valueReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// For and while nodes
case class ForInNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WhileNode(condReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Misc
case class GlobalNode(variable: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

case class AssertIterable(reg: Int, length: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)