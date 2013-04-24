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
case class WriteVariableNode(variable: String, value_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WritePropertyNode(base_reg: Int, property: String, value_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WriteIndexableNode(base_reg: Int, property_reg: Int, value_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

case class ConstantBooleanNode(result_reg: Int, value: Boolean, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantIntNode(result_reg: Int, value: PyInteger, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantFloatNode(result_reg: Int, value: PyFloat, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantLongNode(result_reg: Int, value: PyLong, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantComplexNode(result_reg: Int, value: PyComplex, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantStringNode(result_reg: Int, value: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ConstantNoneNode(result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

case class NewListNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class NewDictionaryNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class NewTupleNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class NewSetNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Read variable; result = variable
// Read property from base (object/class); result =  base.property
// Read property from dictionary: result = base[property]
case class ReadVariableNode(variable: String, result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ReadPropertyNode(base_reg: Int, property: String, result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ReadIndexableNode(base_reg: Int, property_reg: Int, result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Del
case class DelVariableNode(variable: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class DelIndexableNode(base_reg: Int, property_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class DelPropertyNode(base_reg: Int, property: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// No Operation node; pass
case class NoOpNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Break node
case class BreakNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// If statement: if (condition): then_block else: else_block
case class IfNode(condition_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Return statement: return result
case class ReturnNode(result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Exceptional Return; 
case class ExceptionalReturnNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(result_reg: Int, base_reg: Option[Int], function_reg: Int, argument_regs: List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Raise error; Raise value
case class RaiseNode(value_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Except an exception; except [(]types[)] [as result]: 
case class ExceptNode(types: List[String], result_reg: Option[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Binary operation; result = arg1 op arg2
case class BinOpNode(op: constants.BinOp.Value, arg1_reg: Int, arg2_reg: Int, result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class BoolOpNode(op: constants.BoolOp.Value, leftReg: Int, rightReg: Int, resultReg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class UnOpNode(op: constants.UnOp.Value, arg1_reg: Int, result_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Print node; print value
case class PrintNode(value_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// For and while nodes
case class ForInNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WhileNode(cond_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Misc
case class GlobalNode(variable: String, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

case class AssertIterable(reg: Int, length: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)