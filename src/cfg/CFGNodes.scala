package tapy.cfg

import java.util.UUID
import tapy.constants

abstract class Node(label: String, id: String)

// Class and function declaration (exit and entry)
case class EntryNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class ExitNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Write variable; variable = value
case class WriteVariableNode(variable:String,value_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Read variable; result = variable
case class ReadVariableNode(variable:String,result_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Write property into base (object/class); base.property = value
case class WritePropertyNode(base_reg:Int,property:String,value_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Read property from base (object/class); result =  base.property
case class ReadPropertyNode(base_reg:Int,property:String,result_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Write property into dictionary: base[property] = value
case class WriteDictionaryNode(base_reg:Int,property_reg:Int,value_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Read property from dictionary: result = base[property]
case class ReadDictionaryNode(base_reg:Int,property_reg:Int,result_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// No Operation node; pass
case class NoOpNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Break node
case class BreakNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// If statement: if (condition): then_block else: else_block
case class IfNode(condition_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Return statement: return result
case class ReturnNode(result_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Exceptional Return; 
case class ExceptionalReturnNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Function invokation and Object creation calls; result = [base.]function(arguments)
case class CallNode(result_reg:Int,base_reg:Option[Int],function_reg:Int,argument_regs:List[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Raise error; Raise value
case class RaiseNode(value_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Except an exception; except [(]types[)] [as result]:
case class ExceptNode(types:List[String],result_reg:Option[Int], label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Binary operation; result = arg1 op arg2
case class BinOpNode(op: constants.BinOp.Value,arg1_reg:Int,arg2_reg:Int,result_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Unary operation: result = op arg1
case class UnaryOpNode(op: constants.UnOp.Value,arg1_reg:Int,result_reg:Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// Print node; print value
case class PrintNode(value_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)

// For and while nodes
case class ForInNode(label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)
case class WhileNode(cond_reg: Int, label: String, id: String = UUID.randomUUID().toString()) extends Node(label, id)