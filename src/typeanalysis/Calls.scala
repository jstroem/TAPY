package tapy.typeanalysis

import tapy.constants.StackConstants
import java.lang.ArithmeticException
import org.python.antlr.ast.arguments
import org.python.antlr.ast.Name
import tapy.dfa._
import tapy.dfa.MonotoneFrameworkTypes._
import tapy.cfg._
import tapy.lattices._
import tapy.exceptions._
import tapy.constants
import scala.collection.JavaConversions._

trait Calls extends Exceptions with Logger {
  var worklist: Worklist[AnalysisLattice.Elt]
  
  def handleCallNode(node: CallNode, solution: Elt): Elt = {
    try {
      val afterCallNode = worklist.cfg.getSuccessors(node).head.asInstanceOf[AfterCallNode]
      
      // Clear the return registers
      var res = node.updateStackFrames(solution, Set((StackConstants.RETURN, ValueLattice.bottom), (StackConstants.RETURN_CONSTRUCTOR, ValueLattice.bottom)), true)
    
      val function = node.getRegisterValue(solution, node.functionReg)
      val labels = ValueLattice.getObjectLabels(function)
      
      if (!ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](function)) {
        res = Exceptions.raiseNewBuiltInException(node, "TypeError", res, true)
      }
      
      if (labels.size > 1) {
        throw new NotImplementedException("Possibly calling more than one function not implemented")
      }
      
      val result = labels.foldLeft(res) {(acc, label) =>
        val obj = HeapLattice.getObject(node.getHeap(solution), label)
        label match {
          case label: NewStyleClassObjectLabel => handleClassObjectCall(node, afterCallNode, label, obj, acc)
          case label: OldStyleClassObjectLabel => handleClassObjectCall(node, afterCallNode, label, obj, acc)
          case label: FunctionObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
          case label: BoundMethodObjectLabel => handleBoundMethodObjectCall(node, afterCallNode, label, acc)
          case label: WrapperObjectLabel =>
            label.label match {
              case label: FunctionObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
              // case label: UnboundMethodObjectLabel => handleFunctionObjectCall(node, afterCallNode, label, acc)
            }
            
          case _ =>
            throw new TypeError("Trying to call a non-callable object")
        }
      }
      
      result
      
    } catch {
      case e: UnexpectedValueException =>
        log("CallNode", e.getMessage())
        node.setState(solution)
        
      case e: TypeError =>
        log("CallNode", e.getMessage())
        node.setState(solution)
        
      case e: NotImplementedException =>
        log("CallNode", e.getMessage())
        node.setState(solution)
    }
  }
  
  def handleClassObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, classLabel: ClassObjectLabel, classObj: ObjectLattice.Elt, solution: Elt): Elt = {
    // Construct the object
    val instanceLabel = classLabel match {
      case label: NewStyleClassObjectLabel => NewStyleInstanceObjectLabel(label, callNode)
      case label: OldStyleClassObjectLabel => OldStyleInstanceObjectLabel(label, callNode)
      case label => throw new InternalError()
    }
    
    val instanceValue = ValueLattice.setObjectLabels(Set(instanceLabel))
    
    var tmp = callNode.updateHeap(solution, instanceLabel, ObjectLattice.setProperty("__class__", PropertyLattice.setValue(ValueLattice.setObjectLabels(Set(classLabel)))))

    tmp = ObjectLattice.getProperties(classObj) match {
      case PropertiesLattice.Top() => throw new NotImplementedException()
      case PropertiesLattice.Concrete(classProperties) =>
        classProperties.foldLeft(tmp) {(acc, entry) =>
          val (property, propertyElt) = entry
          val value = PropertyLattice.getValue(propertyElt)
          
          // First we write all the non-object values.
          // Note! Don't write propertyElt to the acc: a local may be declared to be global in the class,
          // but it will not be global for its instances!
          val tmp = Utils.writePropertyValueOnObjectLabelToHeap(callNode, property, instanceLabel, ValueLattice.setObjectLabels(Set(), value), acc)
          
          // Second we write all the object values
          ValueLattice.getObjectLabels(value).foldLeft(tmp) {(acc, valueLabel) =>
            valueLabel match {
              case valueLabel: UnboundMethodObjectLabel =>
                val functionValue = ValueLattice.setObjectLabels(Set(valueLabel))
                
                val methodLabel = BoundMethodObjectLabel(instanceLabel, valueLabel.functionLabel)
                val methodValue = ValueLattice.setObjectLabels(Set(methodLabel))
                val methodObject = ObjectLattice.updatePropertyValue("*function*", functionValue)
                
                val tmp = callNode.updateHeap(acc, methodLabel, methodObject)
                Utils.writePropertyValueOnObjectLabelToHeap(callNode, property, instanceLabel, methodValue, tmp)
                
              case valueLabel =>
                throw new NotImplementedException()
            }
          }
        }
      
      case _ =>
        throw new InternalError()
    }
    
    val instanceObj = callNode.getObject(tmp, instanceLabel)
    
    val init = ObjectLattice.getPropertyValue(instanceObj, "__init__")
    if (init == ValueLattice.bottom) {
      // __init__ is not defined
      tmp = AnalysisLattice.updateCallGraph(tmp, Set((null, callNode, null, afterCallNode, false, true)))
      
      callNode.updateStackFrame(tmp, StackConstants.RETURN_CONSTRUCTOR, instanceValue)
      
    } else if (ValueLattice.elementIsOnlyObjectLabels[ObjectLabel](init)) {
      // __init__ is defined
      ValueLattice.getObjectLabels(init).foldLeft(tmp) {(acc, initLabel) =>
        initLabel match {
          case initLabel: BoundMethodObjectLabel =>
            tmp = handleFunctionArguments(callNode, initLabel.functionLabel, tmp, Some(ValueLattice.setObjectLabels(Set(initLabel.instance))))
            
            // Normal call edges
            tmp = AnalysisLattice.updateCallGraph(tmp,
              Set((null, callNode, null, initLabel.functionLabel.entryNode, false, true),
                  (null, initLabel.functionLabel.exitNode, null, afterCallNode, false, true)))
            
            // Exception call edges
            worklist.cfg.getExceptionSuccessors(callNode).foldLeft(tmp) {(acc, succ) =>
              AnalysisLattice.updateCallGraph(acc, Set((null, initLabel.functionLabel.exceptionalExitNode, null, succ, false, false)))
            }
            
            callNode.updateStackFrame(tmp, StackConstants.RETURN_CONSTRUCTOR, instanceValue)
            
          case initLabel =>
            throw new NotImplementedException("TypeError: Trying to call a non-function object")
        }
      }
      
    } else {
      throw new NotImplementedException("TypeError: Trying to call non-callable object")
    }
  }
  
  def handleFunctionObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, functionLabel: FunctionObjectLabel, solution: Elt): Elt = {
    var tmp = handleFunctionArguments(callNode, functionLabel, solution)
    
    // Normal call edges
    tmp = AnalysisLattice.updateCallGraph(tmp,
      Set((null, callNode, null, functionLabel.entryNode, true, true),
          (null, functionLabel.exitNode, null, afterCallNode, true, true)))
          
    // Exception call edges
    worklist.cfg.getExceptionSuccessors(callNode).foldLeft(tmp) {(acc, succ) =>
      AnalysisLattice.updateCallGraph(acc, Set((null, functionLabel.exceptionalExitNode, null, succ, true, false)))
    }
  }
  
  def handleBoundMethodObjectCall(callNode: CallNode, afterCallNode: AfterCallNode, methodLabel: BoundMethodObjectLabel, solution: Elt): Elt = {
    var tmp = handleFunctionArguments(callNode, methodLabel.functionLabel, solution, Some(ValueLattice.setObjectLabels(Set(methodLabel.instance)))) // TODO
    
    // Normal call edges
    tmp = AnalysisLattice.updateCallGraph(tmp,
      Set((null, callNode, null, methodLabel.functionLabel.entryNode, true, true),
          (null, methodLabel.functionLabel.exitNode, null, afterCallNode, true, true)))
    
    // Exception call edges
    worklist.cfg.getExceptionSuccessors(callNode).foldLeft(tmp) {(acc, succ) =>
      AnalysisLattice.updateCallGraph(acc, Set((null, methodLabel.functionLabel.exceptionalExitNode, null, succ, true, false)))
    }
  }
  
  /* Sets the argument-registers given to the callNode on the functionObjectScope with the correct naming */
  def handleFunctionArguments(callNode: CallNode, functionLabel: FunctionObjectLabel, solution: Elt, receiver: Option[ValueLattice.Elt] = None): Elt = {
    var functionScopeObject = HeapLattice.getObject(callNode.getHeap(solution), functionLabel.scopeLabel)
    
    if (callNode.keywordRegs.size > 0)
      throw new NotImplementedException("Keywords on function calls is not implemented");
    else if (callNode.starArgReg != None)
      throw new NotImplementedException("Star arguments on function calls is not implemented");
    else if (callNode.kwArgReg != None)
      throw new NotImplementedException("kw arguments on function calls is not implemented");

    // Set the parameters as variables in the functionScopeObject
    var args: List[String] = List()
    if (functionLabel.entryNode.funcDef != null) // null if default __init__
      args = functionLabel.entryNode.funcDef.getInternalArgs().getInternalArgs().toList.map(_ match {
        case t: Name => t.getInternalId()
        case _ => throw new NotImplementedException("Other elements than Name was used as arguments in function definition")
      })
    else if (functionLabel.entryNode.name == "__init__")
      args = List("self")
    
    // If the argument size is not equal an exception should potentially be rasied
    if (args.size - functionLabel.declNode.defaultArgRegs.size > callNode.argRegs.size + receiver.size) {
     throw new NotImplementedException("List of registers given as arguments to function is smaller than required argument length")
    }

    val argsWithoutReceiver = if (receiver.size == 1) args.tail else args // Remove "self"
    val argRegPairs = argsWithoutReceiver.zipWithIndex.map({case (arg,idx) => 
      if (callNode.argRegs.size > idx) {
        (arg, callNode.argRegs(idx))
      } else {
        (arg, functionLabel.declNode.defaultArgRegs(idx - callNode.argRegs.size))
      }
    })

    functionScopeObject = receiver match {
      case Some(receiver) => Utils.writePropertyValueOnObject(functionScopeObject, args(0), receiver, false)
      case None => functionScopeObject
    }
    
    functionScopeObject = argRegPairs.foldLeft(functionScopeObject) {(acc,pair) =>
      val (argName,reg) = pair
      Utils.writePropertyValueOnObject(acc, argName, callNode.getRegisterValue(solution, reg), false)
    }
    
    callNode.updateHeap(solution, functionLabel.scopeLabel, functionScopeObject)
  }
  
  /**
   * joinPredecessors() does not join constructor call edges, so we do that here!
   * 
   * handleAfterCallNode must remember to clear the return registers:
   * Otherwise, x = A(), where A.__init__ creates an object of type B(), would
   * result in x being either an object of type A or B.
   */
  def handleAfterCallNode(node: AfterCallNode, solution: Elt): Elt = {
    try {
      // Join constructor call edges!
      var state = CallGraphLattice.getConstructorCallPredecessors(AnalysisLattice.getCallGraph(solution), node).foldLeft(StateLattice.bottom) {(acc, pred) =>
        log("AfterCallNode", "Joining from " + pred)
        
        // Check that __init__ returns None
        pred match {
          case pred: FunctionExitNode =>
            val initReturnValue = StackFrameLattice.getRegisterValue(pred.getStackFrame(solution), StackConstants.RETURN)
            if (!ValueLattice.elementIsOnlyNone(initReturnValue)) {
              throw new TypeError("__init__() should return None (actual: " + initReturnValue + ")")
            }
            
          case _ => // Happens when there is no __init__
        }
        
        StateLattice.leastUpperBound(acc, pred.getState(solution))
      }

      state = StateLattice.setExecutionContext(state)
      state = StateLattice.leastUpperBound(node.getState(solution), state)
      state = StateLattice.updateStackFrame(state, StackConstants.RETURN, ValueLattice.bottom, true)

      // Clear the return register (ensures that a=C() => a=C(), and not A=C() or A=None)
      var tmp = node.setState(solution, state)

      // println()
      // println("New execution context after AfterCallNode: " + node.getExecutionContexts(tmp))
      // println("Old was: " + node.getExecutionContexts(solution))
      // println()
      
      // Get the returned values
      val value = node.getRegisterValues(solution, Set(StackConstants.RETURN, StackConstants.RETURN_CONSTRUCTOR))
      log("AfterCallNode", "Returned value: " + value)

      // Store the returned values and clear the return registers
      node.updateStackFrames(tmp, Set((node.resultReg, value), (StackConstants.RETURN, ValueLattice.bottom), (StackConstants.RETURN_CONSTRUCTOR, ValueLattice.bottom)), true)
      
    } catch {
      case e: TypeError =>
        log("AfterCallNode", "TypeError: __init__() should return None")
        AnalysisLattice.setState(solution, node)
    }
  }
  
  def handleReturnNode(node: ReturnNode, solution: Elt): Elt = {
    val value = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), node.resultReg)
    val oldValue = StackFrameLattice.getRegisterValue(node.getStackFrame(solution), StackConstants.RETURN)
    node.updateStackFrame(solution, StackConstants.RETURN, ValueLattice.leastUpperBound(value, oldValue))
  }
}