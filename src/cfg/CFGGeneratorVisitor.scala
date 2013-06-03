package tapy.cfg

import org.python.core._
import scala.collection.immutable.Set
import tapy.constants
import scala.collection.immutable.List
import tapy.export._
import org.python.antlr.PythonTree
import org.python.antlr.ast._
import org.python.antlr.ast
import org.python.antlr.base._
import scala.collection.JavaConversions._
import tapy.exceptions._
import java.io._

object Registers {
  // Registers
  
  var index :Int  = 0
  def next() : Int = {
    index += 1
    index
  }

  var last : Int = 0
}

class CFGGeneratorVisitor(moduleName: String) extends VisitorBase[ControlFlowGraph] {
  
  /* State */

  // Loops (purpose: continue, break)
  
  var loopEntryNode: Node = null
  var loopExitNode: Node = null

  // Try-except-finally
  
  var finallyNormalCfg: ControlFlowGraph = null
  var finallyHandledCfg: ControlFlowGraph = null
  var finallyUnhandledCfg: ControlFlowGraph = null

  /* Helper methods */

  def generateCFGOfStatementList(entryNode: Node, statements: java.util.List[stmt]): ControlFlowGraph = {
    return statements.toList.foldLeft(new ControlFlowGraph(entryNode)) {(acc, stm) =>
      val stmCfg = stm.accept(this)
      stmCfg.entryNodes.head match {
        case node: ClassEntryNode =>
          // The class body are evaluated at declaration time, so we need to connect it (contrary to functions)
          val classDeclNode = new ClassDeclNode(node, stmCfg.exitNodes.head.asInstanceOf[ClassExitNode], namesToList(node.classDef.getInternalBases().toList))
          val afterClassDeclNode = new NoOpNode("After-class-decl")
          acc.append(classDeclNode).append(stmCfg)
          
        case node: FunctionEntryNode =>
          val (defaultArgCfg, defaultArgRegs) = generateDefaultArguments(node.funcDef.getInternalArgs())
          val (exitNode, exceptionalExitNode) = stmCfg.exitNodes.foldLeft((null: FunctionExitNode, null: ExceptionalExitNode)) {(acc, node) =>
            node match {
              case node: FunctionExitNode => (node, acc._2)
              case node: ExceptionalExitNode => (acc._1, node)
              case node => throw new InternalError()
            }
          }
          acc.insert(stmCfg).append(defaultArgCfg).append(new FunctionDeclNode(node, exitNode, exceptionalExitNode, defaultArgRegs))
          
        case node: BreakNode =>
          acc.append(stmCfg).setExitNodes(Set()) // !
          
        case node =>
          acc.append(stmCfg)
      }
    }
  }

  def generateDefaultArguments(arguments: arguments): (ControlFlowGraph, List[Int]) = {
    arguments.getInternalDefaults().toList.foldLeft((new ControlFlowGraph(new NoOpNode("BoolOp entry")),List[Int]())) {(acc, default) => 
      val (cfg, regs) = acc
      val newCfg = cfg.append(default.accept(this))
      (newCfg, Registers.last :: regs)
    }
  }

  def operatorTypeToBinOp(operator: operatorType): constants.BinOp.Value = operator match {
    case operatorType.UNDEFINED => null
    case operatorType.Add => constants.BinOp.PLUS
    case operatorType.Sub => constants.BinOp.MINUS
    case operatorType.Mult => constants.BinOp.MULT
    case operatorType.Div => constants.BinOp.DIV
    case operatorType.Mod => constants.BinOp.MOD
    case operatorType.Pow => constants.BinOp.POW
    case operatorType.LShift => constants.BinOp.SHL
    case operatorType.RShift => constants.BinOp.SHR
    case operatorType.BitOr => constants.BinOp.OR
    case operatorType.BitXor => constants.BinOp.XOR
    case operatorType.BitAnd => constants.BinOp.AND
    case operatorType.FloorDiv => constants.BinOp.IDIV
  }
  
  def boolopTypeToBoolOp(operator: boolopType): constants.BoolOp.Value = operator match {
    case boolopType.UNDEFINED => null
    case boolopType.And => constants.BoolOp.AND
    case boolopType.Or => constants.BoolOp.OR
  }

  def unaryopTypeToUnOp(operator: unaryopType): constants.UnOp.Value = operator match {
    case unaryopType.UNDEFINED => null
    case unaryopType.Invert => constants.UnOp.TILDE
    case unaryopType.Not => constants.UnOp.NOT
    case unaryopType.UAdd => constants.UnOp.PLUS
    case unaryopType.USub => constants.UnOp.MINUS
  }

  /* Abstract methods from VisitorBase */

  override def traverse(node: PythonTree): Unit = {
    node.traverse(this)
  }

  override def unhandled_node(node: PythonTree): ControlFlowGraph = {
    return null
  }

  /* Implementation of visitor methods: */

  override def visitModule(node: Module): ControlFlowGraph = {
    println("visitModule")
    if (moduleName != "__builtin__") {
      val body = generateCFGOfStatementList(new ImportNode(List("__builtin__"), true), node.getInternalBody())
      new ControlFlowGraph(new ModuleEntryNode(moduleName, node)).append(body)
    } else
      generateCFGOfStatementList(new ModuleEntryNode(moduleName, node), node.getInternalBody())
  }

  override def visitInteractive(node: Interactive): ControlFlowGraph = {
    println("visitInteractive")
    return null
  }

  override def visitExpression(node: Expression): ControlFlowGraph = {
    println("visitExpression")
    return null
  }

  override def visitSuite(node: Suite): ControlFlowGraph = {
    println("visitSuite")
    return null
  }

  // Note: FunctionDeclNode inserted in generateCFGOfStatementList
  override def visitFunctionDef(node: FunctionDef): ControlFlowGraph = {
    println("visitFunctionDef")
    
    val entryNode = FunctionEntryNode(node.getInternalName(), node)
    val exitNode = FunctionExitNode(node.getInternalName(), entryNode)
    val exceptionalExitNode = ExceptionalExitNode(node.getInternalName(), entryNode)
    
    val bodyCfg = generateCFGOfStatementList(entryNode, node.getInternalBody())
    
    // Insert explicit return None node
    val noneRegister = Registers.next()
    val returnNoneCfg = new ControlFlowGraph(new ConstantNoneNode(noneRegister))
      .append(new ReturnNode(noneRegister))
    
    val funcCfg = bodyCfg.exitNodes.foldLeft(bodyCfg.append(exitNode).addNode(exceptionalExitNode)) {(acc, node) => node match {
        case node: ReturnNode => acc
        case node => acc.removeEdges(node, exitNode).insert(returnNoneCfg, node, exitNode)
      }
    }
    
    // Connect nodes to the exceptional exit node
    funcCfg.addExitNode(exceptionalExitNode).connectExcept(funcCfg.nodes - entryNode - exitNode - exceptionalExitNode, exceptionalExitNode)
  }

  // Note: ClassDeclNode inserted in generateCFGOfStatementList
  override def visitClassDef(node: ClassDef): ControlFlowGraph = {
    println("visitClassDef")
    
    val entryNode = ClassEntryNode(node.getInternalName(), namesToList(node.getInternalBases().toList), node)
    val exitNode = ClassExitNode(node.getInternalName(), entryNode)
    
    val bodyCfg = generateCFGOfStatementList(entryNode, node.getInternalBody())
    return bodyCfg.append(exitNode)
  }

  override def visitReturn(node: Return): ControlFlowGraph = {
    if (node.getInternalValue() == null) {
      val noneRegister = Registers.next()
      return new ControlFlowGraph(new ConstantNoneNode(noneRegister)).append(new ReturnNode(noneRegister))
      
    } else {
      val expressionCfg = node.getInternalValue().accept(this)
      val expressionRegister = Registers.last
      return expressionCfg.append(new ReturnNode(expressionRegister))
    }
  }

  override def visitDelete(node: Delete): ControlFlowGraph = {
    return node.getInternalTargets().toList.foldLeft(new ControlFlowGraph(new NoOpNode("Del entry"))) {(acc, target) =>
      acc.append(visitDeleteAux(List(target)))
    }
  }
  
  def visitDeleteAux(elts: List[expr]): ControlFlowGraph = {
    return elts.foldLeft(new ControlFlowGraph(new NoOpNode("Del tuple entry"))) {(acc, el) =>
      // A) Make the CFG for this particular assignment
      val elDelCfg = el match {
        case node: Name => new ControlFlowGraph(new DelVariableNode(node.getInternalId()))
        case node: Subscript =>
          val lookupBaseCfg = node.getInternalValue().accept(this)
          val baseRegister = Registers.last
          
          val lookupPropertyCfg = node.getInternalSlice().accept(this)
          val propertyRegister = Registers.last
          
          lookupBaseCfg.append(lookupPropertyCfg).append(new DelIndexableNode(baseRegister, propertyRegister))
        
        case node: Attribute =>
          val lookupCfg = node.getInternalValue().accept(this)
          val lookupRegister = Registers.last
          lookupCfg.append(new DelPropertyNode(lookupRegister, node.getInternalAttr()))
          
        case node: Tuple => visitDeleteAux(node.getInternalElts().toList)
        case node: ast.List => visitDeleteAux(node.getInternalElts().toList)
        case node => throw new NotImplementedException()
      }
      
      // B) Add it to the assignment CFG
      acc.append(elDelCfg)
    }
  }
  
  /*
   * Normalize
   *   x_1 = ... = x_k = exp
   * into (if k >= 2)
   *   tmp = exp
   *   x_k = tmp
   *   ...
   *   x_1 = tmp
   */
  override def visitAssign(node: Assign): ControlFlowGraph = {
    val valueCfg = node.getInternalValue().accept(this)
    val assignCfg = visitAssign(node.getInternalTargets().toList)
    return valueCfg.append(assignCfg)
  }
  
  def visitAssign(targets: List[expr]): ControlFlowGraph = {
    val tmpVariableRegister = Registers.last
    
    var i = 0
    return targets.foldRight(new ControlFlowGraph(new NoOpNode("Assign entry"))) {(target, acc) =>
      // 1) Generate the CFG for a single target (which may be a tuple or list)
      val targetCfg = target match {
        case t: Name => new ControlFlowGraph(new WriteVariableNode(t.getInternalId(), tmpVariableRegister)) 
        case t: Subscript => visitSubscript(t, tmpVariableRegister)
        case t: Attribute => visitAttribute(t, tmpVariableRegister) 
        case t: Tuple => visitAssignAux(tmpVariableRegister, t.getInternalElts())
        case t: ast.List => visitAssignAux(tmpVariableRegister, t.getInternalElts())
        case t => throw new NotImplementedException("")
      }
      i = i + 1
      
      // 2) Combine the accumulator with the generated CFG of the single assignment
      acc.append(targetCfg)
    }
  }
  
  def visitAssignAux(tmpVariableRegister: Int, elts: java.util.List[expr]): ControlFlowGraph = {
    var i = 0
    return elts.toList.foldLeft(new ControlFlowGraph(new NoOpNode("Assign tuple entry"))) {(acc, el) =>
      val indexRegister = Registers.next()
      val indexNode = new ConstantIntNode(indexRegister, new PyInteger(i))
      
      val lookupRegister = Registers.next()
      val lookupNode = new ReadIndexableNode(tmpVariableRegister, indexRegister, lookupRegister)
      
      val lookupCfg = new ControlFlowGraph(indexNode).append(lookupNode)
          
      // A) Make the CFG for this particular assignment
      val elAssignCfg = el match {
        case t: Name => lookupCfg.append(new WriteVariableNode(t.getInternalId(), lookupRegister))
        case t: Subscript => lookupCfg.append(visitSubscript(t, lookupRegister))
        case t: Attribute => lookupCfg.append(visitAttribute(t, lookupRegister))
        case t: Tuple => lookupCfg.append(visitAssignAux(lookupRegister, t.getInternalElts()))
        case t: ast.List => lookupCfg.append(visitAssignAux(lookupRegister, t.getInternalElts()))
        case t => throw new NotImplementedException()
      }
      i = i + 1
      
      // B) Add it to the assignment CFG
      acc.append(elAssignCfg)
    }
  }

  override def visitAugAssign(node: AugAssign): ControlFlowGraph = {
    println("visitAugAssign")
    
    val targetCfg = node.getInternalTarget().accept(this)
    val targetRegister = Registers.last
    
    val valueCfg = node.getInternalValue().accept(this)
    val valueRegister = Registers.last
    
    val writeNode = new BinOpNode(node.getInternalOp(), targetRegister, valueRegister, targetRegister)
    
    return targetCfg.append(valueCfg).append(writeNode)
  }

  override def visitPrint(node: Print): ControlFlowGraph = {
    val exprCfg = if (node.getInternalDest() != null) node.getInternalDest().accept(this) else new ControlFlowGraph(new NoOpNode("No Dest"))
    val destReg = if (node.getInternalDest() != null) Some(Registers.last) else None
    val valueCfgs = node.getInternalValues().toList.map((a) => {
      val cfg = a.accept(this)
      val reg = Registers.last
      (cfg,reg)
    })
    val (cfg,valueRegs) = valueCfgs.foldLeft((exprCfg,List()) : (ControlFlowGraph,List[Int]))((acc,a) => {
      val (cfg,reg) = a
      val (accCfg, valueRegs) = acc
      (accCfg.append(cfg), reg :: valueRegs)
    })

    return cfg.append(new PrintNode(destReg, valueRegs))
  }

  override def visitFor(node: For): ControlFlowGraph = {
    println("visitFor")

    var iterCfg = node.getInternalIter().accept(this)
    val containerReg = Registers.last

    val createIterFunctionReg = Registers.next()
    val iterReg = Registers.next()
    val nextObjFunctionReg = Registers.next()
    val nextObjReg = Registers.next()

    Registers.last = nextObjReg
    val assignCfg = visitAssign(List(node.getInternalTarget()))

    val loopStartNode = new CallNode(nextObjFunctionReg, List(), Map())
    val loopStart = new ControlFlowGraph(loopStartNode).append(new AfterCallNode(nextObjReg))
    val exceptNode = new ExceptNode(List("StopIteration"), List())
    val forExitNode = new NoOpNode("For exit")

    val forOrElseCfg = generateCFGOfStatementList(new NoOpNode("For-or-else entry"), node.getInternalOrelse())

    loopEntryNode = loopStartNode
    loopExitNode = forExitNode

    val forBodyCfg = generateCFGOfStatementList(new NoOpNode("For-body entry"), node.getInternalBody())

    iterCfg = iterCfg.append(new ReadPropertyNode(containerReg, "__iter__", createIterFunctionReg))
                     .append(new CallNode(createIterFunctionReg, List(), Map()))
                     .append(new AfterCallNode(iterReg))
                     .append(new ReadPropertyNode(iterReg, "next", nextObjFunctionReg))
                     .append(loopStart)
                     .addNode(forExitNode)
                     .append(assignCfg)

    //Make exception state: If there is a StopIteration exception, the exceptNode is reached and afterwards going into the orElseCfg
    iterCfg = iterCfg.addNode(exceptNode)
                     .connectExcept(loopStartNode, exceptNode)
                     .insert(forOrElseCfg, exceptNode, forExitNode)


    //Add the forbody onto the cfg
    return iterCfg.insert(forBodyCfg, iterCfg.exitNodes, loopStartNode)
                  .setExitNode(forExitNode)
  }

  override def visitWhile(node: While): ControlFlowGraph = {
    println("visitWhile")

    val conditionCfg = node.getInternalTest().accept(this)
    val conditionRegister = Registers.last
    
    val whileEntryNode = new IfNode(conditionRegister)
    val whileExitNode = new NoOpNode("While exit")
    
    val oldLoopEntryNode = this.loopEntryNode // In case of nested loops
    val oldLoopExitNode = this.loopExitNode

    val whileBodyCfg = generateCFGOfStatementList(new NoOpNode("While-body entry"), node.getInternalBody())
    val whileOrElseCfg = generateCFGOfStatementList(new NoOpNode("While-or-else entry"), node.getInternalOrelse())
    
    this.loopEntryNode = oldLoopEntryNode // Recover in case of nested loops
    this.loopExitNode = oldLoopExitNode
    
    return conditionCfg.append(whileEntryNode)
                       .append(whileOrElseCfg)
                       .append(whileExitNode)
                       .insert(whileBodyCfg, whileEntryNode, conditionCfg.entryNodes)
  }

  override def visitIf(node: If): ControlFlowGraph = {
    println("visitIf")
    
    val conditionCfg = node.getInternalTest().accept(this)
    val conditionRegister = Registers.last
    
    val ifEntryNode: IfNode = new IfNode(conditionRegister)
    val ifExitNode = new NoOpNode("If exit")

    // Construct the CFG's for the two branches
    var thenCfg = generateCFGOfStatementList(new AssertNode(conditionRegister), node.getInternalBody())
    var elseCfg = generateCFGOfStatementList(new AssertNode(conditionRegister, true), node.getInternalOrelse())

    return conditionCfg.append(ifEntryNode)
                       .append(Set(thenCfg,
                                   elseCfg))
  }

  override def visitWith(node: With): ControlFlowGraph = {
    println("visitWith")
    var initCfg = node.getInternalContext_expr().accept(this)
    val objectReg = Registers.last
    val exitFuncReg = Registers.next()
    val enterFuncReg = Registers.next()
    val enterResReg = Registers.next()
    initCfg = initCfg.append(new ReadPropertyNode(objectReg, "__exit__",exitFuncReg))
                     .append(new ReadPropertyNode(objectReg, "__enter__",enterFuncReg))
                     .append(new CallNode(enterFuncReg, List()))
                     .append(new AfterCallNode(enterResReg))

    //If there is a 'as' assign into this result
    if (node.getInternalOptional_vars() != null){
      Registers.last = enterResReg
      initCfg = initCfg.append(visitAssign(List(node.getInternalOptional_vars())))
    }

    var bodyCfg = generateCFGOfStatementList(new NoOpNode("For-body entry"), node.getInternalBody())

    //Create except cfg
    val exitResReg = Registers.next()
    val ifExitResNode = new IfNode(exitResReg) 
    val exceptHandlerCfg = new ControlFlowGraph(new ExceptNode(List(), List()))
                            .append(new CallNode(exitFuncReg, List(constants.StackConstants.EXCEPTION_TYPE, constants.StackConstants.EXCEPTION, constants.StackConstants.TRACE)))
                            .append(new AfterCallNode(exitResReg))
                            .append(ifExitResNode)
                            .append(new RaiseNode(None)) //If the ifExit should go to the next except
                            
    val NoneReg = Registers.next()
    val normalExit = new ControlFlowGraph(new ConstantNoneNode(NoneReg))
                              .append(new CallNode(enterFuncReg, List(NoneReg, NoneReg, NoneReg)))
                              .append(new AfterCallNode(Registers.next()))

    val exitNode = new NoOpNode("After with")


    bodyCfg = bodyCfg.connectExcept(exceptHandlerCfg)
                     .append(normalExit)
                     .append(exitNode)
                     .connect(ifExitResNode, exitNode) //If the ifCheck returns true the exitNode is reached.


    return initCfg.append(bodyCfg)
  }

  override def visitRaise(node: Raise): ControlFlowGraph = {
    println("visitRaise")
    val cfg = node.getInternalType().accept(this)
    return cfg.append(new RaiseNode(Some(Registers.last)))
  }

  override def visitTryExcept(node: TryExcept): ControlFlowGraph = {
    println("visitTryExcept")
    
    val bodyCfg = generateCFGOfStatementList(new NoOpNode("Try-Except entry"), node.getInternalBody())
    
    var lastHandlerEntries: Set[Node] = null // Should only be a single ExceptNode
    val handlerCfg = node.getInternalHandlers().foldLeft(new ControlFlowGraph(new NoOpNode("Try-except-handler entry"))) {(acc, el) =>
      val elCfg = el.accept(this)
      
      val result =
        if (lastHandlerEntries == null)
          acc.insert(elCfg)
             .connect(acc.exitNodes, elCfg.entryNodes) // Connect "Try-except-handler-entry" to elCfg
             .setExitNodes(elCfg.exitNodes) // setExitNodes (not add) s.t. "Try-except-handler-entry" does not become an exit node
        else
          acc.insert(elCfg)
             .connectExcept(lastHandlerEntries, elCfg.entryNodes, true)
             .addExitNodes(elCfg.exitNodes)
      
      lastHandlerEntries = elCfg.entryNodes
      result
    }
    
    val elseCfg = generateCFGOfStatementList(NoOpNode("Try-except-else-entry"), node.getInternalOrelse())
    
    // finallyNormalCfg is null if and only if finallyHandledCfg is null
    if (this.finallyNormalCfg == null && this.finallyHandledCfg == null)
      return bodyCfg.append(elseCfg)
                    .insert(handlerCfg)
                    .connectExcept(bodyCfg.nodes, handlerCfg.entryNodes)
                    .setExitNodes(elseCfg.exitNodes ++ handlerCfg.exitNodes)
    else
      return bodyCfg.append(elseCfg)
                    .insert(handlerCfg.append(this.finallyHandledCfg))
                    .connectExcept(bodyCfg.nodes, handlerCfg.entryNodes)
                    .setExitNodes(elseCfg.exitNodes)
  }

  override def visitTryFinally(node: TryFinally): ControlFlowGraph = {
    println("visitTryFinally")
    
    val finallyNormalCfg = generateCFGOfStatementList(new NoOpNode("Finally-normal entry"), node.getInternalFinalbody())
    val finallyHandledCfg = generateCFGOfStatementList(new NoOpNode("Finally-except-handled entry"), node.getInternalFinalbody())
    val finallyUnhandledCfg = generateCFGOfStatementList(new NoOpNode("Finally-except-unhandled entry"), node.getInternalFinalbody())
    
    val oldFinallyNormalCfg = this.finallyNormalCfg
    val oldFinallyHandledCfg = this.finallyHandledCfg
    val oldFinallyUnhandledCfg = this.finallyUnhandledCfg
    
    this.finallyNormalCfg = finallyNormalCfg
    this.finallyHandledCfg = finallyHandledCfg
    this.finallyUnhandledCfg = finallyUnhandledCfg
    
    val bodyCfg = generateCFGOfStatementList(new NoOpNode("Try-finally entry"), node.getInternalBody())
    
    val result = node.getInternalBody().get(0) match {
      case _: TryExcept =>
        bodyCfg.append(finallyNormalCfg)
               .insert(finallyHandledCfg)
               .insert(finallyUnhandledCfg)
               .connectExcept(bodyCfg.nodes -- finallyNormalCfg.nodes -- finallyHandledCfg.nodes -- finallyUnhandledCfg.nodes, finallyUnhandledCfg.entryNodes)
      case _ =>
        bodyCfg.append(finallyNormalCfg)
               .insert(finallyHandledCfg)
               .insert(finallyUnhandledCfg)
               .connectExcept(bodyCfg.nodes -- finallyNormalCfg.nodes -- finallyHandledCfg.nodes -- finallyUnhandledCfg.nodes, finallyUnhandledCfg.entryNodes)
    }
    
    this.finallyNormalCfg = oldFinallyNormalCfg
    this.finallyHandledCfg = oldFinallyHandledCfg
    this.finallyUnhandledCfg = oldFinallyUnhandledCfg
    
    return result.setExitNodes(finallyNormalCfg.exitNodes ++ finallyHandledCfg.exitNodes)
  }

  override def visitAssert(node: Assert): ControlFlowGraph = {
    println("visitAssert")
    val debugVarReg = Registers.next()
    val exitNode = new NoOpNode("assert merge node")
    val ifDebugNode = new IfNode(debugVarReg)
    val testCfg = node.getInternalTest().accept(this)
    val testReg = Registers.last
    val ifTestNode = new IfNode(testReg)

    val assertionErrorFuncReg = Registers.next() 
    val assertionErrorReg = Registers.next()
    val assertCfg = if (node.getInternalMsg() != null) {
      val msgCfg = node.getInternalMsg().accept(this)
      val msgReg = Registers.last
      msgCfg.append(new ReadVariableNode("AssertionError",assertionErrorFuncReg,true))
            .append(new CallNode(assertionErrorFuncReg, List(msgReg)))
            .append(new AfterCallNode(assertionErrorReg))

    } else {
      new ControlFlowGraph(new ReadVariableNode("AssertionError",assertionErrorFuncReg,true))
        .append(new CallNode(assertionErrorFuncReg, List()))
        .append(new AfterCallNode(assertionErrorReg))
    }
    new ControlFlowGraph(new ReadVariableNode("__debug__",debugVarReg,true)).append(ifDebugNode)
                                                                            .append(testCfg)
                                                                            .append(ifTestNode)
                                                                            .append(assertCfg)
                                                                            .append(new RaiseNode(Some(assertionErrorReg)))
                                                                            .append(exitNode)
                                                                            .connect(ifDebugNode,exitNode)
                                                                            .connect(ifTestNode,exitNode)
  }

  override def visitImport(node: Import): ControlFlowGraph = {
    println("visitImport")
    val names = node.getInternalNames().toList.foldRight(List[String]()) {(name, acc) =>
      name.getInternalName() :: acc
    }
    return new ControlFlowGraph(new ImportNode(names))
  }

  override def visitImportFrom(node: ImportFrom): ControlFlowGraph = {
    println("visitImportFrom")
    return null
  }

  override def visitExec(node: Exec): ControlFlowGraph = {
    println("visitExec")
    return null
  }

  override def visitGlobal(node: Global): ControlFlowGraph = {
    println("visitGlobal")
    return node.getInternalNameNodes().foldLeft(new ControlFlowGraph(new NoOpNode("Global entry"))) {(acc, name) =>
      acc.append(new GlobalNode(name.getInternalId()))
    }
  }

  override def visitExpr(node: Expr): ControlFlowGraph = {
    println("visitExpr")
    return node.getInternalValue().accept(this)
  }

  override def visitPass(node: Pass): ControlFlowGraph = {
    return new ControlFlowGraph(new NoOpNode("pass"))
  }

  override def visitBreak(node: Break): ControlFlowGraph = {
    if (this.loopExitNode != null) {
      // Notice: The break CFG node does not actually contain
      // the for exit CFG node, but the visitFor/visitWhile-method handles this
      val breakNode = new BreakNode("Break")
      return new ControlFlowGraph(breakNode).connect(breakNode, this.loopExitNode)
    }
    throw new InternalError("Break statement outside for or while loop.")
  }

  override def visitContinue(node: Continue): ControlFlowGraph = {
    if (this.loopEntryNode != null) {
      // Notice: The break CFG node does not actually contain
      // the for exit CFG node, but the visitFor/visitWhile-method handles this
      val continueNode = new BreakNode("Continue")
      return new ControlFlowGraph(continueNode).connect(continueNode, this.loopEntryNode)
    }
    throw new InternalError("Continue statement outside for or while loop.")
  }

  override def visitBoolOp(node: ast.BoolOp): ControlFlowGraph = {
    val exprs = node.getInternalValues().toList
    val result_reg = Registers.next()
    val ifExitNode = new NoOpNode("If exit")

    val cfgsAndRegisters : List[(ControlFlowGraph, Int)] = exprs.map((a) => {
      val cfg = a.accept(this)
      val reg = Registers.last
      (cfg,reg)
    })
    val foldedCfg = cfgsAndRegisters.foldLeft(new ControlFlowGraph(new NoOpNode("BoolOp entry")))((acc,a) => {
      var (cfg,reg) = a
      var readValue = acc.append(cfg)
      val untilIfCfg = readValue.append(new IfNode(reg))
      val oneSideNode = boolopTypeToBoolOp(node.getInternalOp()) match {
        case constants.BoolOp.AND => new ConstantBooleanNode(result_reg, false)
        case constants.BoolOp.OR => new ConstantBooleanNode(result_reg, true)
      }
      untilIfCfg.append(oneSideNode).append(ifExitNode)
    })
    val otherSideNode = boolopTypeToBoolOp(node.getInternalOp()) match {
      case constants.BoolOp.AND => new ConstantBooleanNode(result_reg, true)
      case constants.BoolOp.OR => new ConstantBooleanNode(result_reg, false)
    }

    Registers.last = result_reg

    return foldedCfg.append(otherSideNode).append(ifExitNode)
  }

  override def visitBinOp(node: BinOp): ControlFlowGraph = {
    val leftCfg = node.getInternalLeft().accept(this)
    val leftRegister = Registers.last
    
    val rightCfg = node.getInternalRight().accept(this)
    val rightRegister = Registers.last
    
    val resultRegister = Registers.next()
    val binOpNode = new BinOpNode(node.getInternalOp(), leftRegister, rightRegister, resultRegister)

    Registers.last = resultRegister

    return leftCfg.append(rightCfg).append(binOpNode)
  }

  override def visitUnaryOp(node: UnaryOp): ControlFlowGraph = {
    val cfg = node.getInternalOperand().accept(this)
    val register = Registers.last
    val resultRegister = Registers.next()
    Registers.last = resultRegister
    return cfg.append(new UnOpNode(node.getInternalOp(), register, resultRegister))
  }

  override def visitLambda(node: Lambda): ControlFlowGraph = {
    println("visitLambda")
    return null
  }

  override def visitIfExp(node: IfExp): ControlFlowGraph = {
    println("visitIfExp")
    
    val conditionCfg = node.getInternalTest().accept(this)
    val conditionRegister = Registers.last
    
    val ifNode = new IfNode(conditionRegister)
    val ifExitNode = new NoOpNode("If exit")
    
    val thenCfg = node.getInternalBody().accept(this)
    val thenRegister = Registers.last
    
    val elseCfg = node.getInternalOrelse().accept(this)
    val elseRegister = Registers.last
    
    val resultRegister = Registers.next()
    Registers.last = resultRegister
    
    return conditionCfg.append(ifNode)
                       .append(Set(thenCfg.append(new WriteRegisterNode(resultRegister, thenRegister)),
                                   elseCfg.append(new WriteRegisterNode(resultRegister, elseRegister))))
                       .append(ifExitNode)
  }

  override def visitDict(node: Dict): ControlFlowGraph = {
    println("visitDict")
    
    val emptyDictRegister = Registers.next()
    val newDictFunc = Registers.next()
    val emptyDictCfg =
      new ControlFlowGraph(new ReadVariableNode("dict",newDictFunc,true))
        .append(new CallNode(newDictFunc, List()))
        .append(new AfterCallNode(emptyDictRegister))
    
    val dictCfg = node.getInternalKeys().toList.zip(node.getInternalValues().toList).foldLeft(emptyDictCfg) {(acc,entry) =>
      val keyCfg = entry._1.accept(this)
      val keyRegister = Registers.last
      
      val valueCfg = entry._2.accept(this)
      val valueRegister = Registers.last
      
      val writeNode = new WriteIndexableNode(emptyDictRegister, keyRegister, valueRegister)
      
      acc.append(keyCfg).append(valueCfg).append(writeNode)
    }
    Registers.last = emptyDictRegister
    
    return dictCfg
  }

  override def visitSet(node: org.python.antlr.ast.Set): ControlFlowGraph = {
    println("visitSet")
    
    val emptySetRegister = Registers.next()
    val newSetFunc = Registers.next()
    val newSetCfg =
      new ControlFlowGraph(new ReadVariableNode("set",newSetFunc,true))
        .append(new CallNode(newSetFunc, List()))
        .append(new AfterCallNode(emptySetRegister))

    if (node.getInternalElts().size() == 0) {
      // Just return the empty set
      Registers.last = emptySetRegister
      return newSetCfg
      
    } else {
      // Recursively get the CFG parts for the elements
      val pair = node.getInternalElts().toList.map((el) => {
        val elCfg = el.accept(this)
        val elRegister = Registers.last
        (elCfg, elRegister)
      })
      
      Registers.last = emptySetRegister
  
      val setAddFuncRegister = Registers.next()
      val readGetFuncNode = new ReadPropertyNode(emptySetRegister, "add", setAddFuncRegister)

      val newSetAndAddFuncCfg = newSetCfg.append(readGetFuncNode)
      return pair.foldLeft(newSetAndAddFuncCfg)((accCfg, a) => {
        val (elCfg, elRegister) = a
        accCfg.append(elCfg)
              .append(new CallNode(setAddFuncRegister, List(elRegister), Map()))
              .append(new AfterCallNode(Registers.next()))
      })
    }
  }

  def visitComprehension(comp: comprehension, inner: ControlFlowGraph): ControlFlowGraph = {
    var iterCfg = comp.getInternalIter().accept(this)
    val containerReg = Registers.last

    val createIterFunctionReg = Registers.next()
    val iterReg = Registers.next()
    val nextObjFunctionReg = Registers.next()
    val nextObjReg = Registers.next()

    Registers.last = nextObjReg
    val assignCfg = visitAssign(List(comp.getInternalTarget()))

    val loopStartNode = new CallNode(nextObjFunctionReg, List(), Map())
    val loopStart = new ControlFlowGraph(loopStartNode).append(new AfterCallNode(nextObjReg))
    val exceptNode = new ExceptNode(List("StopIteration"), List())
    val forExitNode = new NoOpNode("ForComp exit")


    val ifCfg = comp.getInternalIfs.foldLeft(loopStart)((cfg,ifs) => {
      val acfg = ifs.accept(this)
      val ifNode = new IfNode(Registers.last)
      cfg.append(acfg).append(ifNode).connect(ifNode, loopStartNode)
    })

    iterCfg.append(new ReadPropertyNode(containerReg, "__iter__", createIterFunctionReg))
           .append(new CallNode(createIterFunctionReg, List(), Map()))
           .append(new AfterCallNode(iterReg))
           .append(new ReadPropertyNode(iterReg, "next", nextObjFunctionReg))
           .append(ifCfg)
           .append(assignCfg)
           .append(inner)
           .connect(inner.exitNodes, loopStartNode)
           .addNode(exceptNode)
           .addNode(forExitNode)
           .connectExcept(loopStartNode, exceptNode)
           .connect(exceptNode, forExitNode)
           .setExitNode(forExitNode)
  }

  override def visitListComp(node: ListComp): ControlFlowGraph = {
    println("visitListComp")
    val resultReg = Registers.next()
    val appendFuncReg = Registers.next()
    val newListFunc = Registers.next()
    val newListCfg =
      new ControlFlowGraph(new ReadVariableNode("list",newListFunc,true))
        .append(new CallNode(newListFunc, List()))
        .append(new AfterCallNode(resultReg))
        .append(new ReadPropertyNode(resultReg, "append", appendFuncReg))

    val exprCfg =
      node.getInternalElt().accept(this)
        .append(new CallNode(appendFuncReg, List(Registers.last), Map()))
        .append(new AfterCallNode(Registers.next()))

    val resCfg = node.getInternalGenerators().reverse.foldLeft(exprCfg)((cfg,comp) => visitComprehension(comp,cfg))

    Registers.last = resultReg

    return newListCfg.append(resCfg)
  }

  override def visitSetComp(node: SetComp): ControlFlowGraph = {
    println("visitSetComp")
    val resultReg = Registers.next()
    val appendFuncReg = Registers.next()
    val newSetFunc = Registers.next()
    val newSetCfg =
      new ControlFlowGraph(new ReadVariableNode("set",newSetFunc,true))
        .append(new CallNode(newSetFunc, List()))
        .append(new AfterCallNode(resultReg))
        .append(new ReadPropertyNode(resultReg, "add", appendFuncReg))

    val exprCfg = node.getInternalElt().accept(this)
      .append(new CallNode(appendFuncReg, List(Registers.last), Map()))
      .append(new AfterCallNode(Registers.next()))

    val resCfg = node.getInternalGenerators().reverse.foldLeft(exprCfg)((cfg,comp) => visitComprehension(comp,cfg))

    Registers.last = resultReg

    return newSetCfg.append(resCfg)
  }

  override def visitDictComp(node: DictComp): ControlFlowGraph = {
    println("visitDictComp")
    val resultReg = Registers.next()
    val appendFuncReg = Registers.next()
    val newDictFunc = Registers.next()
    val newDictCfg = new ControlFlowGraph(new ReadVariableNode("dict",newDictFunc,true))
      .append(new CallNode(newDictFunc, List()))
      .append(new AfterCallNode(resultReg))
      .append(new ReadPropertyNode(resultReg, "add", appendFuncReg))

    val keyCfg = node.getInternalKey().accept(this)
    val keyReg = Registers.last

    val valueCfg = node.getInternalValue().accept(this)
    val valueReg = Registers.last
    
    val exprCfg = keyCfg.append(valueCfg).append(new WriteIndexableNode(resultReg, keyReg, valueReg))

    val resCfg = node.getInternalGenerators().reverse.foldLeft(exprCfg)((cfg,comp) => visitComprehension(comp,cfg))

    Registers.last = resultReg

    return newDictCfg.append(resCfg)
  }

  override def visitGeneratorExp(node: GeneratorExp): ControlFlowGraph = {
    println("visitGeneratorExp")
    return null
  }

  override def visitYield(node: Yield): ControlFlowGraph = {
    println("visitYield")
    return null
  }
  
  override def visitCompare(node: Compare): ControlFlowGraph = {
    println("visitCompare")
    
    val pairs = node.getInternalComparators().toList.zip(node.getInternalOps().toList)
    
    val resultRegister = Registers.next()
    val exitNode = new NoOpNode("Compare exit")
    
    var i = 0
    val initialAcc = node.getInternalLeft().accept(this).addNode(exitNode)
    val result = pairs.foldLeft(initialAcc) {(acc, pair) =>
      val (exp, op) = pair
      
      val prevExpRegister = Registers.last
      
      val expCfg = exp.accept(this)
      val expRegister = Registers.last
      
      val compareOpNode = new CompareOpNode(op, prevExpRegister, expRegister, resultRegister)
      
      i = i + 1
      if (i < pairs.size()) {
        val ifNode = new IfNode(resultRegister)
        acc.append(expCfg).append(compareOpNode).append(ifNode).connect(ifNode, exitNode)
      } else {
        // Last iteration: Don't add an if-node, instead add an edge from the last comparison-node to the exit-node
        acc.append(expCfg).append(compareOpNode).connect(compareOpNode, exitNode)
      }
    }
    
    Registers.last = resultRegister
    return result.setExitNode(exitNode)
  }
  
  override def visitCall(node: Call): ControlFlowGraph = {
    println("visitCall")
    
    // Lookup the function
    val lookupCfg = node.getInternalFunc().accept(this)
    val lookupRegister = Registers.last
    
    // Lookup the arguments
    var argsRegisters = List[Int]()
    val argsCfg = node.getInternalArgs().toList.foldLeft(new ControlFlowGraph(new NoOpNode("Function-arguments entry"))) {(acc, el) =>
      val elCfg = el.accept(this)
      argsRegisters = Registers.last :: argsRegisters
      
      acc.append(elCfg)
    }
    
    // Lookup the keyword arguments
    var keywordsRegisters = Map[String, Int]()
    val keywordsCfg = node.getInternalKeywords().toList.foldLeft(new ControlFlowGraph(new NoOpNode("Keyword-arguments entry"))) {(acc, el) =>
      val elCfg = el.getInternalValue().accept(this)
      keywordsRegisters = keywordsRegisters + (el.getInternalArg() -> Registers.last)
      
      acc.append(elCfg)
    }
    
    // Lookup the stararg arguments
    val (starargCfg, starargRegister) =
      if (node.getInternalStarargs() != null)
        (node.getInternalStarargs().accept(this), Some(Registers.last))
      else
        (new ControlFlowGraph(new NoOpNode("No starargs")), None)
    
    // Lookup the kw arguments
    val (kwargCfg, kwargRegister) =
      if (node.getInternalKwargs() != null)
        (node.getInternalKwargs().accept(this), Some(Registers.last))
      else
        (new ControlFlowGraph(new NoOpNode("No starargs")), None)
    
    // Call the function with the arguments
    val resultRegister = Registers.next()
    Registers.last = resultRegister
    
    lookupCfg.append(argsCfg).append(keywordsCfg).append(starargCfg).append(kwargCfg)
      .append(new CallNode(lookupRegister, argsRegisters.reverse, keywordsRegisters, starargRegister, kwargRegister))
      .append(new AfterCallNode(resultRegister))
  }

  override def visitRepr(node: Repr): ControlFlowGraph = {
    println("visitRepr")
    val exprCfg = node.getInternalValue().accept(this)
    val exprReg = Registers.last
    val resultReg = Registers.next()
    val reprFuncReg = Registers.next()

    Registers.last = resultReg
    return exprCfg.append(new ReadVariableNode("repr", reprFuncReg, true))
                  .append(new CallNode(reprFuncReg, List(exprReg)))
                  .append(new AfterCallNode(resultReg))
  }

  override def visitNum(node: Num): ControlFlowGraph = {
    val numRegister = Registers.next()
    Registers.last = numRegister
    return node.getInternalN() match {
      case pyInt: PyInteger => new ControlFlowGraph(new ConstantIntNode(numRegister, pyInt))
      case pyLong: PyLong => new ControlFlowGraph(new ConstantLongNode(numRegister, pyLong))
      case pyFloat: PyFloat => new ControlFlowGraph(new ConstantFloatNode(numRegister, pyFloat))
      case pyComplex: PyComplex => new ControlFlowGraph(new ConstantComplexNode(numRegister, pyComplex))
    }
  }

  override def visitStr(node: Str): ControlFlowGraph = {
    val strRegister = Registers.next()
    Registers.last = strRegister
    return new ControlFlowGraph(new ConstantStringNode(strRegister, node.getInternalS().toString()))
  }

  override def visitAttribute(node: Attribute): ControlFlowGraph = {
    visitAttribute(node, -1)
  }
  
  def visitAttribute(node: Attribute, assignFromRegister: Int): ControlFlowGraph = {
    println("visitAttribute")
    val lastExpressionRegister = Registers.last
    
    val lookupCfg = node.getInternalValue().accept(this)
    val lookupRegister = Registers.last
    
    val readRegister = Registers.next()
    val attributeCfg =
      if (assignFromRegister >= 0)
        new ControlFlowGraph(new WritePropertyNode(lookupRegister, node.getInternalAttr(), assignFromRegister))
      else
        new ControlFlowGraph(new ReadPropertyNode(lookupRegister, node.getInternalAttr(), readRegister))
    Registers.last = if (assignFromRegister >= 0) lastExpressionRegister else readRegister
    
    return lookupCfg.append(attributeCfg)
  }

  override def visitSubscript(node: Subscript): ControlFlowGraph = {
    return visitSubscript(node, -1)
  }
  
  def visitSubscript(node: Subscript, assignFromRegister: Int): ControlFlowGraph = {
    println("visitSubscript")
    
    val lastExpressionRegister = Registers.last
    
    val lookupBaseCfg = node.getInternalValue().accept(this)
    val baseRegister = Registers.last
    
    val lookupPropertyCfg = node.getInternalSlice().accept(this)
    val propertyRegister = Registers.last
    
    val readRegister = Registers.next()
    val subscriptNode =
      if (assignFromRegister >= 0)
        new WriteIndexableNode(baseRegister, propertyRegister, assignFromRegister)
      else
        new ReadIndexableNode(baseRegister, propertyRegister, readRegister)
    Registers.last = if (assignFromRegister >= 0) lastExpressionRegister else readRegister
    
    lookupBaseCfg.append(lookupPropertyCfg).append(subscriptNode)
  }

  override def visitName(node: Name): ControlFlowGraph = {
    val nameRegister = Registers.next()
    Registers.last = nameRegister
    return new ControlFlowGraph(new ReadVariableNode(node.getInternalId(), nameRegister))
  }

  override def visitList(node: ast.List): ControlFlowGraph = {
    println("visitList")
    val resultReg = Registers.next()
    val newListFunc = Registers.next()
    val newListCfg = new ControlFlowGraph(new ReadVariableNode("list",newListFunc,true))
      .append(new CallNode(newListFunc, List()))
      .append(new AfterCallNode(resultReg))


    val pair = node.getInternalElts().toList.map((a) => {
      val cfg = a.accept(this)
      val reg = Registers.last
      (cfg,reg)
    })

    Registers.last = resultReg

    if (pair.length > 0) {
      val appendFuncReg = Registers.next()
      val readAppendFuncNode = ReadPropertyNode(resultReg, "append", appendFuncReg)

      val newListAndAppendFuncCfg = newListCfg.addNode(readAppendFuncNode).connect(newListCfg.exitNodes, readAppendFuncNode).setExitNode(readAppendFuncNode)
      return pair.foldLeft(newListAndAppendFuncCfg)((accCfg,a) => {
        val (cfg,reg) = a
        accCfg.append(cfg)
              .append(new CallNode(appendFuncReg, List(reg), Map()))
              .append(new AfterCallNode(Registers.next()))
      })
    } else return newListCfg
  }

  override def visitTuple(node: Tuple): ControlFlowGraph = {
    println("visitTuple")    

    val listResultReg = Registers.next()
    val newListFunc = Registers.next()
    val appendFuncReg = Registers.next()
    val newListCfg = new ControlFlowGraph(new ReadVariableNode("list",newListFunc,true))
      .append(new CallNode(newListFunc, List()))
      .append(new AfterCallNode(listResultReg))
      .append(new ReadPropertyNode(listResultReg, "append", appendFuncReg))
    
    val valuesCfg = node.getInternalElts().toList.foldLeft(newListCfg){(acc, el) =>
      val elCfg = el.accept(this)
      val elReg = Registers.last
      acc.append(elCfg.append(new CallNode(appendFuncReg, List(elReg)))
                      .append(new AfterCallNode(Registers.next())))
    }
    
    val tupleRegister = Registers.next()
    val newTupleFunc = Registers.next()
    Registers.last = tupleRegister

    
    return valuesCfg
      .append(new ReadVariableNode("tuple", newTupleFunc, true))
      .append(new CallNode(newTupleFunc, List(listResultReg)))
      .append(new AfterCallNode(tupleRegister))
  }
  
  def namesToList(elts: List[expr], acc: List[String] = List()): List[String] = {
    elts.foldRight(acc) {(exp, acc) =>
      exp match {
        case name: Name => name.getInternalId() :: acc
        case list: ast.List => namesToList(list.getInternalElts().toList, acc)
        case tuple: Tuple => namesToList(tuple.getInternalElts().toList, acc)
      }
    }
  }

  override def visitEllipsis(node: Ellipsis): ControlFlowGraph = {
    println("visitEllipsis")
    Registers.last = Registers.next()
    return new ControlFlowGraph(new ReadVariableNode("Ellipsis", Registers.last, true))
  }

  override def visitSlice(node: Slice): ControlFlowGraph = {
    println("visitSlice")
    val resultReg = Registers.next()
    val newSliceFunc = Registers.next()
    var argsList : List[Int] = List()
    var lowerCfg = new ControlFlowGraph(new NoOpNode("No lower"))
    var upperCfg = new ControlFlowGraph(new NoOpNode("No upper"))
    var stepCfg = new ControlFlowGraph(new NoOpNode("No step"))
    if (node.getInternalLower() != null) {
      lowerCfg = node.getInternalLower().accept(this)
      argsList = Registers.last :: argsList
    }
    if (node.getInternalUpper() != null) {
      upperCfg = node.getInternalUpper().accept(this)
      argsList = Registers.last :: argsList
    }
    if (node.getInternalStep() != null) {
      stepCfg = node.getInternalStep().accept(this)
      argsList = Registers.last :: argsList 
    }

    Registers.last = resultReg
    return lowerCfg.append(upperCfg)
                   .append(stepCfg)
                   .append(new ReadVariableNode("slice",newSliceFunc,true))
                   .append(new CallNode(newSliceFunc, argsList.reverse))
                   .append(new AfterCallNode(resultReg))
  }

  override def visitExtSlice(node: ExtSlice): ControlFlowGraph = {
    println("visitExtSlice")

    val listResultReg = Registers.next()
    val newListFunc = Registers.next()
    val appendFuncReg = Registers.next()
    val newListCfg = new ControlFlowGraph(new ReadVariableNode("list",newListFunc,true))
      .append(new CallNode(newListFunc, List()))
      .append(new AfterCallNode(listResultReg))
      .append(new ReadPropertyNode(listResultReg, "append", appendFuncReg))
    
    val slicesCfg = node.getInternalDims().toList.foldLeft(newListCfg){(acc, el) =>
      val sliceCfg = el.accept(this)
      val sliceReg = Registers.last
      acc.append(sliceCfg)
         .append(new CallNode(appendFuncReg, List(sliceReg)))
         .append(new AfterCallNode(Registers.next()))
    }
    
    val tupleRegister = Registers.next()
    val newTupleFunc = Registers.next()
    Registers.last = tupleRegister
    
    return slicesCfg.append(new ReadVariableNode("tuple", newTupleFunc, true))
                    .append(new CallNode(newTupleFunc, List(listResultReg)))
                    .append(new AfterCallNode(tupleRegister))
  }

  override def visitIndex(node: Index): ControlFlowGraph = {
    println("visitIndex")

    return node.getInternalValue().accept(this)
  }

  override def visitExceptHandler(node: ExceptHandler): ControlFlowGraph = {
    println("visitExceptHandler")
    
    val excType: List[String] = if (node.getInternalType() != null) namesToList(List(node.getInternalType())) else List()
    val excName: List[String] = if (node.getInternalName() != null) namesToList(List(node.getInternalName())) else List()
    val excNode = new ExceptNode(excType, excName)
    
    return generateCFGOfStatementList(excNode, node.getInternalBody())
  }
}
