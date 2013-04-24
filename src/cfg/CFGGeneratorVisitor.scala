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
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import java.io._

object CFGGeneratorVisitor extends VisitorBase[ControlFlowGraph] {

  /* State */

  var forEntryCfgNode: Node = null
  var forExitCfgNode: Node = null
  
  var whileEntryCfgNode: Node = null
  var whileExitCfgNode: Node = null

  var nextTempVariableIndex : Int = 0
  def nextTempVariable(): String = {
    nextTempVariableIndex = nextTempVariableIndex + 1
    return s"_tmp$nextTempVariableIndex"
  }

  var nextRegisterIndex :Int  = 0
  def nextRegister() : Int = {
    nextRegisterIndex += 1
    nextRegisterIndex
  }

  var lastExpressionRegister : Int = 0

  /* Helper methods */

  def generateCFGOfStatementList(entryNode: Node, statements: java.util.List[stmt]): ControlFlowGraph = {
    return statements.toList.foldLeft(ControlFlowGraph.makeSingleton(entryNode)) {(acc, stm) =>
      val stmCfg = stm.accept(this)
      stmCfg.entryNodes.head match {
        case node: EntryNode =>
          acc.combineGraphs(stmCfg)
             .setEntryNodes(acc.entryNodes)
             .setExitNodes(acc.exitNodes)
        case node: BreakNode =>
          acc.combineGraphs(stmCfg)
             .setEntryNodes(acc.entryNodes)
             .setExitNodes(Set()) // !
             .connectNodes(acc.exitNodes, stmCfg.entryNodes)
        case node =>
          acc.combineGraphs(stmCfg)
             .setEntryNodes(acc.entryNodes)
             .setExitNodes(stmCfg.exitNodes)
             .connectNodes(acc.exitNodes, stmCfg.entryNodes)
      }
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
    return generateCFGOfStatementList(new NoOpNode("Program entry"), node.getInternalBody())
  }

  override def visitInteractive(node: Interactive): ControlFlowGraph = {
    println("visitInteractive")
    return null
  }

  override def visitExpression(node: Expression): ControlFlowGraph = {
    println("visitExpression");
    return null
  }

  override def visitSuite(node: Suite): ControlFlowGraph = {
    println("visitSuite");
    return null
  }

  override def visitFunctionDef(node: FunctionDef): ControlFlowGraph = {
    println("visitFunctionDef")

    val entryCfgNode = new EntryNode(node.getInternalName())
    val exitCfgNode = new ExitNode(node.getInternalName())

    val bodyCfg = generateCFGOfStatementList(entryCfgNode, node.getInternalBody())

    // No need to add entryCfgNode, as this has already been added to bodyCfg (and set to entry node)
    return bodyCfg.addNodes(Set(exitCfgNode))
      .setExitNode(exitCfgNode)
      .connectNodes(bodyCfg.exitNodes, exitCfgNode)
  }

  override def visitClassDef(node: ClassDef): ControlFlowGraph = {
    println("visitClassDef")
    val entryCfgNode = new EntryNode(node.getInternalName())
    val exitCfgNode = new ExitNode(node.getInternalName())

    val bodyCfg = generateCFGOfStatementList(entryCfgNode, node.getInternalBody())

    // No need to add entryCfgNode, as this has already been added to bodyCfg (and set to entry node)
    return bodyCfg.addNodes(Set(exitCfgNode))
      .setExitNode(exitCfgNode)
      .connectNodes(bodyCfg.exitNodes, exitCfgNode)
  }

  override def visitReturn(node: Return): ControlFlowGraph = {
    val exprCfg = node.getInternalValue().accept(this)
    val returnNode = new ReturnNode(this.lastExpressionRegister, node.accept(ASTPrettyPrinter))
    return exprCfg.addNode(returnNode)
                  .connectNodes(exprCfg.exitNodes, returnNode)
                  .setEntryNodes(exprCfg.entryNodes)
                  .setExitNode(returnNode)
  }

  override def visitDelete(node: Delete): ControlFlowGraph = {
    return node.getInternalTargets().toList.foldLeft(ControlFlowGraph.makeSingleton(new NoOpNode("Del entry"))) {(acc, target) =>
      val targetCfg = target match {
        case t: Name =>
          ControlFlowGraph.makeSingleton(new DelVariableNode(t.getInternalId(), t.accept(ASTPrettyPrinter)))
          
        case t: Subscript =>
          ControlFlowGraph.makeSingleton(new DelIndexableNode(0, 0, t.accept(ASTPrettyPrinter)))
          
        case t: Attribute =>
          ControlFlowGraph.makeSingleton(new DelPropertyNode(0, t.getInternalAttr(), t.accept(ASTPrettyPrinter)))
          
        case t: ast.List =>
          visitDeleteAux(t.getInternalElts())
          
        case t: Tuple =>
          visitDeleteAux(t.getInternalElts())
          
        case t =>
          throw new NotImplementedException()
      }
      
      // 2) Combine the accumulator with the generated CFG of the single assignment
      acc.combineGraphs(targetCfg)
         .connectNodes(acc.exitNodes, targetCfg.entryNodes)
         .setEntryNodes(acc.entryNodes)
         .setExitNodes(targetCfg.exitNodes)
    }
  }
  
  def visitDeleteAux(elts: java.util.List[expr]): ControlFlowGraph = {
    return elts.toList.foldLeft(ControlFlowGraph.makeSingleton(new NoOpNode("Del tuple entry"))) {(acc, el) =>
      // A) Make the CFG for this particular assignment
      val elDelCfg = el match {
        case t: Name =>
          ControlFlowGraph.makeSingleton(new DelVariableNode(t.getInternalId(), t.getInternalId()))
          
        case t: Subscript =>
          ControlFlowGraph.makeSingleton(new DelIndexableNode(0, 0, t.accept(ASTPrettyPrinter)))
          
        case t: Attribute =>
          ControlFlowGraph.makeSingleton(new DelPropertyNode(0, t.getInternalAttr(), t.accept(ASTPrettyPrinter)))
        
        case t: Tuple =>
          visitDeleteAux(t.getInternalElts())
        
        case t: ast.List =>
          visitDeleteAux(t.getInternalElts())
        
        case t =>
          throw new NotImplementedException()
      }
      
      // B) Add it to the assignment CFG
      acc.combineGraphs(elDelCfg)
         .connectNodes(acc.exitNodes, elDelCfg.entryNodes)
         .setEntryNodes(acc.entryNodes)
         .setExitNodes(elDelCfg.exitNodes)
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
  // TODO add asserts
  override def visitAssign(node: Assign): ControlFlowGraph = {
    val targets = node.getInternalTargets()
    
    // Create a temporary variable to store the expression
    val tmpVariableName = nextTempVariable()
    val tmpVariableCfg = node.getInternalValue().accept(this)
    val tmpVariableRegister = this.lastExpressionRegister
    
    
    
    // Important to use foldRight, such that x_k is taken first
    var i = 0
    println(targets.size())
    return targets.toList.foldRight(tmpVariableCfg) {(target, acc) =>
      // 1) Generate the CFG for a single target (which may be a tuple or list)
      val targetCfg = target match {
        case t: Name =>
          ControlFlowGraph.makeSingleton(new WriteVariableNode(t.getInternalId(), tmpVariableRegister, tmpVariableName))
        
        case t: Subscript =>
          visitSubscript(t, tmpVariableRegister)
        
        case t: Attribute =>
          ControlFlowGraph.makeSingleton(new WritePropertyNode(0, t.getInternalAttr(), 0, tmpVariableName))
        
        case t: Tuple => 
          visitAssignAux(node, tmpVariableRegister, t.getInternalElts())
  
        case t: ast.List =>
          visitAssignAux(node, tmpVariableRegister, t.getInternalElts())
          
        case t =>
          throw new NotImplementedException()
      }
      
      i = i + 1
      
      // 2) Combine the accumulator with the generated CFG of the single assignment
      acc.combineGraphs(targetCfg)
         .connectNodes(acc.exitNodes, targetCfg.entryNodes)
         .setEntryNodes(acc.entryNodes)
         .setExitNodes(targetCfg.exitNodes)
    }
  }
  
  def visitAssignAux(node: Assign, tmpVariableRegister: Int, elts: java.util.List[expr], indexes: List[Int] = List()): ControlFlowGraph = {
    val indexesStr = indexes.foldLeft("") {(acc, index) => s"[$index]$acc"}
    
    var i = 0
    return elts.toList.foldLeft(ControlFlowGraph.makeSingleton(new NoOpNode("Assign tuple entry"))) {(acc, el) =>
      // A) Make the CFG for this particular assignment
      val elAssignCfg = el match {
        case t: Name =>
          ControlFlowGraph.makeSingleton(new WriteVariableNode(t.getInternalId(), tmpVariableRegister, ""))
        
        case t: Subscript =>
          visitSubscript(t, tmpVariableRegister)
        
        case t: Attribute =>
          ControlFlowGraph.makeSingleton(new WritePropertyNode(0, t.getInternalAttr(), tmpVariableRegister, ""))
        
        case t: Tuple =>
          visitAssignAux(node, tmpVariableRegister, t.getInternalElts(), i :: indexes)
        
        case t: ast.List =>
          visitAssignAux(node, tmpVariableRegister, t.getInternalElts(), i :: indexes)
        
        case t =>
          throw new NotImplementedException()
      }
      
      i = i + 1
      
      // B) Add it to the assignment CFG
      acc.combineGraphs(elAssignCfg)
         .connectNodes(acc.exitNodes, elAssignCfg.entryNodes)
         .setEntryNodes(acc.entryNodes)
         .setExitNodes(elAssignCfg.exitNodes)
    }
  }

  override def visitAugAssign(node: AugAssign): ControlFlowGraph = {
    println("visitAugAssign");
    return null
  }

  override def visitPrint(node: Print): ControlFlowGraph = {
    val exprCfg = if (node.getInternalDest() != null) node.getInternalDest().accept(this) else ControlFlowGraph.makeSingleton(new NoOpNode("No Dest"))
    val destReg = if (node.getInternalDest() != null) Some(lastExpressionRegister) else None
    val valueCfgs = node.getInternalValues().toList.map((a) => {
      val cfg = a.accept(this)
      val reg = lastExpressionRegister
      (cfg,reg)
    })
    val (cfg,valueRegs) = valueCfgs.foldLeft((exprCfg,List()) : (ControlFlowGraph,List[Int]))((acc,a) => {
      val (cfg,reg) = a
      val (accCfg,valueRegs) = acc
      val newCfg = accCfg.combineGraphs(cfg)
                         .connectNodes(accCfg.exitNodes, cfg.entryNodes)
                         .setEntryNodes(accCfg.entryNodes)
                         .setExitNodes(cfg.exitNodes)
      (newCfg, reg :: valueRegs)
    })

    val printNode = new PrintNode(destReg, valueRegs, "")
    return cfg.addNode(printNode)
              .connectNodes(cfg.exitNodes, printNode)
              .setExitNode(printNode)
  }

  override def visitFor(node: For): ControlFlowGraph = {
    println("visitFor");

    val forEntryCfgNode = new ForInNode(node.accept(ASTPrettyPrinter))
    val forExitCfgNode = new NoOpNode("For exit")
    val forOrElseEntryCfgNode = new NoOpNode("For else")

    val oldForEntryCfgNode = this.forEntryCfgNode // In case of nested loops
    val oldForExitCfgNode = this.forExitCfgNode
    val oldWhileEntryCfgNode = this.whileEntryCfgNode
    val oldWhileExitCfgNode = this.whileExitCfgNode

    this.forEntryCfgNode = forEntryCfgNode
    this.forExitCfgNode = forExitCfgNode
    this.whileEntryCfgNode = null
    this.whileExitCfgNode = null

    val forBodyCfg = generateCFGOfStatementList(forEntryCfgNode, node.getInternalBody())
    val forOrElseCfg = generateCFGOfStatementList(forOrElseEntryCfgNode, node.getInternalOrelse())
    
    this.forEntryCfgNode = oldForEntryCfgNode // Recover in case of nested loops
    this.forExitCfgNode = oldForExitCfgNode
    this.whileEntryCfgNode = oldWhileEntryCfgNode
    this.whileExitCfgNode = oldWhileExitCfgNode

    val forCfg = forBodyCfg.combineGraphs(forOrElseCfg)
                           .addNode(forExitCfgNode)
                           .connectNodes(forEntryCfgNode, forOrElseEntryCfgNode)
                           .connectNodes(forBodyCfg.exitNodes, forEntryCfgNode)
                           .connectNodes(forOrElseCfg.exitNodes, forExitCfgNode)
                           .setEntryNode(forEntryCfgNode)
                           .setExitNode(forExitCfgNode)

    if (forOrElseCfg.getNodeSuccessors(forOrElseEntryCfgNode).size() == 0) {
      return forCfg.removeNode(forOrElseEntryCfgNode)
    } else {
      return forCfg
    }
  }

  override def visitWhile(node: While): ControlFlowGraph = {
    println("visitWhile");

    val whileEntryCfgNode = new WhileNode(0, s"while ${node.getInternalTest().accept(ASTPrettyPrinter)}: ...")
    val whileExitCfgNode = new NoOpNode("While exit")
    val whileOrElseEntryCfgNode = new NoOpNode("While else")
    
    val oldForEntryCfgNode = this.forEntryCfgNode // In case of nested loops
    val oldForExitCfgNode = this.forExitCfgNode
    val oldWhileEntryCfgNode = this.whileEntryCfgNode
    val oldWhileExitCfgNode = this.whileExitCfgNode

    this.forExitCfgNode = null
    this.whileExitCfgNode = whileExitCfgNode

    val whileBodyCfg = generateCFGOfStatementList(whileEntryCfgNode, node.getInternalBody())
    val whileOrElseCfg = generateCFGOfStatementList(whileOrElseEntryCfgNode, node.getInternalOrelse())
    
    this.forEntryCfgNode = oldForEntryCfgNode // Recover in case of nested loops
    this.forExitCfgNode = oldForExitCfgNode
    this.whileEntryCfgNode = oldWhileEntryCfgNode
    this.whileExitCfgNode = oldWhileExitCfgNode

    val whileCfg = whileBodyCfg.combineGraphs(whileOrElseCfg)
      .addNode(whileExitCfgNode)
      .connectNodes(whileEntryCfgNode, whileOrElseEntryCfgNode)
      .connectNodes(whileBodyCfg.exitNodes, whileEntryCfgNode)
      .connectNodes(whileOrElseCfg.exitNodes, whileExitCfgNode)
      .setEntryNode(whileEntryCfgNode)
      .setExitNode(whileExitCfgNode)

    if (whileOrElseCfg.getNodeSuccessors(whileOrElseEntryCfgNode).size() == 0) {
      return whileCfg.removeNode(whileOrElseEntryCfgNode)
    } else {
      return whileCfg
    }
  }

  override def visitIf(node: If): ControlFlowGraph = {
    println("visitIf")
    val condCfg = node.getInternalTest().accept(this)

    val ifEntryCfgNode: IfNode = new IfNode(lastExpressionRegister, "")
    val ifExitCfgNode = new NoOpNode("If exit")

    // Construct the CFG's for the two branches
    var thenCfg = generateCFGOfStatementList(ifEntryCfgNode, node.getInternalBody())
    var elseCfg = generateCFGOfStatementList(ifEntryCfgNode, node.getInternalOrelse())

    if (elseCfg.getNodeSuccessors(ifEntryCfgNode).size() == 0) {
      // There is no or-else branch
      return thenCfg.combineGraphs(condCfg)
                    .addNode(ifExitCfgNode)
                    .connectNodes(condCfg.exitNodes, ifEntryCfgNode)
                    .connectNodes(ifEntryCfgNode, ifExitCfgNode)
                    .connectNodes(thenCfg.exitNodes, ifExitCfgNode)
                    .setEntryNodes(condCfg.entryNodes)
                    .setExitNode(ifExitCfgNode)
    } else {
      // TODO: Break node must not go to the IfExitNode in ex. 10
      return thenCfg.combineGraphs(elseCfg)
                    .combineGraphs(condCfg)
                    .addNode(ifExitCfgNode)
                    .connectNodes(condCfg.exitNodes, ifEntryCfgNode)
                    .connectNodes(thenCfg.exitNodes ++ elseCfg.exitNodes, ifExitCfgNode)
                    .setEntryNodes(condCfg.entryNodes)
                    .setExitNode(ifExitCfgNode)
    }
  }

  override def visitWith(node: With): ControlFlowGraph = {
    println("visitWith");
    return null
  }

  override def visitRaise(node: Raise): ControlFlowGraph = {
    println("visitRaise");
    val cfg = node.getInternalType().accept(this)
    val raiseNode = new RaiseNode(lastExpressionRegister,s"Raise $lastExpressionRegister")
    return cfg.addNode(raiseNode)
              .connectNodes(cfg.exitNodes, raiseNode)
              .setExitNode(raiseNode)
  }

  override def visitTryExcept(node: TryExcept): ControlFlowGraph = {
    println("visitTryExcept");
    return null
  }

  override def visitTryFinally(node: TryFinally): ControlFlowGraph = {
    println("visitTryFinally");
    return null
  }

  override def visitAssert(node: Assert): ControlFlowGraph = {
    println("visitAssert");
    return null
  }

  override def visitImport(node: Import): ControlFlowGraph = {
    println("visitImport");
    return null
  }

  override def visitImportFrom(node: ImportFrom): ControlFlowGraph = {
    println("visitImportFrom");
    return null
  }

  override def visitExec(node: Exec): ControlFlowGraph = {
    println("visitExec");
    return null
  }

  override def visitGlobal(node: Global): ControlFlowGraph = {
    println("visitGlobal");
    return node.getInternalNameNodes().foldLeft(ControlFlowGraph.makeSingleton(new NoOpNode("Global entry"))) {(acc, name) =>
      val nameGlobalNode = new GlobalNode(name.getInternalId(), name.getInternalId())
      acc.addNode(nameGlobalNode)
         .connectNodes(acc.exitNodes, nameGlobalNode)
         .setExitNode(nameGlobalNode)
    }
  }

  override def visitExpr(node: Expr): ControlFlowGraph = {
    println("visitExpr");
    return node.getInternalValue().accept(this)
  }

  override def visitPass(node: Pass): ControlFlowGraph = {
    return ControlFlowGraph.makeSingleton(new NoOpNode("pass"))
  }

  override def visitBreak(node: Break): ControlFlowGraph = {
    val breakCfgNode = BreakNode("Break")
    val breakCfgNodes = Set[Node](breakCfgNode)
    if (this.forExitCfgNode != null) {
      // Notice: The break CFG node does not actually contain
      // the for exit CFG node, but the visitFor-method handles this
      return new ControlFlowGraph(breakCfgNodes, breakCfgNodes, breakCfgNodes, Map(breakCfgNode -> Set(this.forExitCfgNode)))
    } else if (this.whileExitCfgNode != null) {
      // Notice: The break CFG node does not actually contain
      // the while exit CFG node, but the visitWhile-method handles this
      return new ControlFlowGraph(breakCfgNodes, breakCfgNodes, breakCfgNodes, Map(breakCfgNode -> Set(this.whileExitCfgNode)))
    }
    throw new InternalError("Break statement outside for or while loop.")
  }

  override def visitContinue(node: Continue): ControlFlowGraph = {
    val continueCfgNode = BreakNode("Continue")
    val continueCfgNodes = Set[Node](continueCfgNode)
    if (this.forEntryCfgNode != null) {
      // Notice: The break CFG node does not actually contain
      // the for exit CFG node, but the visitFor-method handles this
      return new ControlFlowGraph(continueCfgNodes, continueCfgNodes, continueCfgNodes, Map(continueCfgNode -> Set(this.forEntryCfgNode)))
    } else if (this.whileEntryCfgNode != null) {
      // Notice: The break CFG node does not actually contain
      // the while exit CFG node, but the visitWhile-method handles this
      return new ControlFlowGraph(continueCfgNodes, continueCfgNodes, continueCfgNodes, Map(continueCfgNode -> Set(this.whileEntryCfgNode)))
    }
    throw new InternalError("Continue statement outside for or while loop.")
  }

  override def visitBoolOp(node: ast.BoolOp): ControlFlowGraph = {
    val exprs = node.getInternalValues().toList
    val result_reg = nextRegister()
    val ifExitNode = new NoOpNode("If exit")

    val cfgsAndRegisters : List[(ControlFlowGraph,Int)] = exprs.map((a) => {
          val cfg = a.accept(this)
          val reg = lastExpressionRegister
          (cfg,reg)
    })
    val foldedCfg = cfgsAndRegisters.foldLeft(ControlFlowGraph.makeSingleton(new NoOpNode("BoolOp entry")))((acc,a) => {
      var (cfg,reg) = a
      var readValue = acc.combineGraphs(cfg)
                         .connectNodes(acc.exitNodes, cfg.entryNodes)
                         .setExitNodes(cfg.exitNodes)
                         .setEntryNodes(acc.entryNodes)
      val ifNode = IfNode(reg, s"if ($reg)")
      val untilIfCfg = readValue.addNode(ifNode)
                                .connectNodes(readValue.exitNodes, ifNode)
                                .setExitNode(ifNode)
      val oneSideNode = boolopTypeToBoolOp(node.getInternalOp()) match {
        case constants.BoolOp.AND => new ConstantBooleanNode(result_reg, false, "False")
        case constants.BoolOp.OR => new ConstantBooleanNode(result_reg, true, "True")
      }
      untilIfCfg.addNode(oneSideNode)
                .connectNodes(untilIfCfg.exitNodes, oneSideNode)
                .addNode(ifExitNode)
                .connectNodes(oneSideNode, ifExitNode)
    })
    val otherSideNode = boolopTypeToBoolOp(node.getInternalOp()) match {
      case constants.BoolOp.AND => new ConstantBooleanNode(result_reg, true, "True")
      case constants.BoolOp.OR => new ConstantBooleanNode(result_reg, false, "False")
    }

    lastExpressionRegister = result_reg

    return foldedCfg.addNode(otherSideNode)
                    .connectNodes(foldedCfg.exitNodes, otherSideNode)
                    .addNode(ifExitNode)
                    .connectNodes(otherSideNode, ifExitNode)
                    .setExitNode(ifExitNode)
  }

  override def visitBinOp(node: BinOp): ControlFlowGraph = {
    val leftCfg = node.getInternalLeft().accept(this)
    val leftRegister = lastExpressionRegister
    val rightCfg = node.getInternalRight().accept(this)
    val rightRegister = lastExpressionRegister
    val resultRegister = nextRegister()
    val binOpNode = new BinOpNode(operatorTypeToBinOp(node.getInternalOp()), leftRegister, rightRegister, resultRegister, node.accept(ASTPrettyPrinter))

    lastExpressionRegister = resultRegister

    return leftCfg.combineGraphs(rightCfg)
                  .connectNodes(leftCfg.exitNodes, rightCfg.entryNodes)
                  .setEntryNodes(leftCfg.entryNodes)
                  .addNode(binOpNode)
                  .connectNodes(rightCfg.exitNodes, binOpNode)
                  .setExitNode(binOpNode)
  }

  override def visitUnaryOp(node: UnaryOp): ControlFlowGraph = {
    val cfg = node.getInternalOperand().accept(this)
    val register = lastExpressionRegister
    val resultRegister = nextRegister()
    val unaryOpNode = new UnOpNode(unaryopTypeToUnOp(node.getInternalOp()), register, resultRegister, node.accept(ASTPrettyPrinter))

    lastExpressionRegister = resultRegister

    return cfg.addNode(unaryOpNode)
              .connectNodes(cfg.exitNodes, unaryOpNode)
              .setExitNode(unaryOpNode)
  }

  override def visitLambda(node: Lambda): ControlFlowGraph = {
    println("visitLambda");
    return null
  }

  override def visitIfExp(node: IfExp): ControlFlowGraph = {
    println("visitIfExp");
    val condCfg = node.getInternalTest().accept(this)
    val resultRegister = nextRegister()
    val ifNode = IfNode(lastExpressionRegister, s"if ($lastExpressionRegister)")
    val ifExitNode = new NoOpNode("If exit")
    val thenCfg = node.getInternalBody().accept(this)
    val writeThenNode = WriteRegisterNode(resultRegister,lastExpressionRegister, "")
    val elseCfg = node.getInternalOrelse().accept(this)
    val writeElseNode = WriteRegisterNode(resultRegister,lastExpressionRegister, "")

    lastExpressionRegister = resultRegister

    return condCfg.addNode(ifNode)
                  .addNode(ifExitNode)
                  .connectNodes(condCfg.exitNodes, ifNode)
                  //Add Then cfg
                  .combineGraphs(thenCfg)
                  .connectNodes(ifNode, thenCfg.entryNodes)
                  .addNode(writeThenNode)
                  .connectNodes(thenCfg.exitNodes, writeThenNode)
                  .connectNodes(writeThenNode, ifExitNode)
                  //add Else cfg
                  .combineGraphs(elseCfg)
                  .connectNodes(ifNode, elseCfg.entryNodes)
                  .addNode(writeElseNode)
                  .connectNodes(elseCfg.exitNodes, writeElseNode)
                  .connectNodes(writeElseNode, ifExitNode)
                  //Set entry and exit nodes
                  .setEntryNodes(condCfg.entryNodes)
                  .setExitNode(ifExitNode)
  }

  override def visitDict(node: Dict): ControlFlowGraph = {
    println("visitDict");
    // TODO
    val emptyDictRegister = nextRegister()
    val emptyDictCfg = ControlFlowGraph.makeSingleton(new NewDictionaryNode(emptyDictRegister, "{}"))
    
    val dictCfg = node.getInternalKeys().toList.zip(node.getInternalValues().toList).foldLeft(emptyDictCfg) {(acc,entry) =>
      val keyCfg = entry._1.accept(this)
      val keyRegister = this.lastExpressionRegister
      
      val valueCfg = entry._2.accept(this)
      val valueRegister = this.lastExpressionRegister
      
      val writeNode = new WriteIndexableNode(emptyDictRegister, keyRegister, valueRegister, "<" + emptyDictRegister + ">[" + entry._1.accept(ASTPrettyPrinter) + "] = " + entry._2.accept(ASTPrettyPrinter))
      
      acc.combineGraphs(keyCfg)
         .combineGraphs(valueCfg)
         .addNode(writeNode)
         .connectNodes(acc.exitNodes, keyCfg.entryNodes)
         .connectNodes(keyCfg.exitNodes, valueCfg.entryNodes)
         .connectNodes(valueCfg.exitNodes, writeNode)
         .setEntryNodes(acc.entryNodes)
         .setExitNode(writeNode)
    }
    this.lastExpressionRegister = emptyDictRegister
    
    return dictCfg
    
  }

  override def visitSet(node: org.python.antlr.ast.Set): ControlFlowGraph = {
    println("visitSet");
    val resultReg = nextRegister()
    val newSetCfg = ControlFlowGraph.makeSingleton(NewSetNode(resultReg, ""))

    val pair = node.getInternalElts().toList.map((a) => {
      val cfg = a.accept(this)
      val reg = lastExpressionRegister
      (cfg,reg)
    })

    lastExpressionRegister = resultReg

    if (pair.length > 0) {
      val addFuncReg = nextRegister()
      val readGetFuncNode = ReadPropertyNode(resultReg, "add", addFuncReg, "")

      val newSetAndGetFuncCfg = newSetCfg.addNode(readGetFuncNode).connectNodes(newSetCfg.exitNodes, readGetFuncNode).setExitNode(readGetFuncNode)
      return pair.foldLeft(newSetAndGetFuncCfg)((accCfg,a) => {
        val (cfg,reg) = a
        val callGetNode = CallNode(nextRegister(), addFuncReg, List(reg), "")
        accCfg.combineGraphs(cfg)
              .connectNodes(accCfg.exitNodes, cfg.entryNodes)
              .addNode(callGetNode)
              .connectNodes(cfg.exitNodes, callGetNode)
              .setEntryNodes(accCfg.entryNodes)
              .setExitNode(callGetNode)
      })
    } else return newSetCfg
  }

  override def visitListComp(node: ListComp): ControlFlowGraph = {
    println("visitListComp");
    return null
  }

  override def visitSetComp(node: SetComp): ControlFlowGraph = {
    println("visitSetComp");
    return null
  }

  override def visitDictComp(node: DictComp): ControlFlowGraph = {
    println("visitDictComp");
    return null
  }

  override def visitGeneratorExp(node: GeneratorExp): ControlFlowGraph = {
    println("visitGeneratorExp");
    return null
  }

  override def visitYield(node: Yield): ControlFlowGraph = {
    println("visitYield");
    return null
  }

  override def visitCompare(node: Compare): ControlFlowGraph = {
    println("visitCompare");
    return null
  }

  override def visitCall(node: Call): ControlFlowGraph = {
    println("visitCall");
    
    // 1) Lookup the function
    val lookupCfg = node.getInternalFunc().accept(this)
    
    // 2) Call it
    val callNode = new CallNode(nextRegister(), lastExpressionRegister, List(), node.accept(ASTPrettyPrinter))
    lookupCfg.addNode(callNode).connectNodes(lookupCfg.exitNodes, callNode).setExitNode(callNode)
  }

  override def visitRepr(node: Repr): ControlFlowGraph = {
    println("visitRepr");
    return null
  }

  override def visitNum(node: Num): ControlFlowGraph = {
    println("visitNum");
    val numRegister = nextRegister()
    this.lastExpressionRegister = numRegister
    return node.getInternalN() match {
      case pyInt: PyInteger => ControlFlowGraph.makeSingleton(new ConstantIntNode(numRegister, pyInt, node.accept(ASTPrettyPrinter)))
      case pyLong: PyLong => ControlFlowGraph.makeSingleton(new ConstantLongNode(numRegister, pyLong, node.accept(ASTPrettyPrinter)))
      case pyFloat: PyFloat => ControlFlowGraph.makeSingleton(new ConstantFloatNode(numRegister, pyFloat, node.accept(ASTPrettyPrinter)))
      case pyComplex: PyComplex => ControlFlowGraph.makeSingleton(new ConstantComplexNode(numRegister, pyComplex, node.accept(ASTPrettyPrinter)))
    }
  }

  override def visitStr(node: Str): ControlFlowGraph = {
    println("visitStr");
    val strRegister = nextRegister()
    this.lastExpressionRegister = strRegister
    return ControlFlowGraph.makeSingleton(new ConstantStringNode(strRegister, node.getInternalS().toString(), node.accept(ASTPrettyPrinter)))
  }

  override def visitAttribute(node: Attribute): ControlFlowGraph = {
    visitAttribute(node, -1)
  }
  
  def visitAttribute(node: Attribute, assignFromRegister: Int): ControlFlowGraph = {
    println("visitAttribute")
    val lastExpressionRegister = this.lastExpressionRegister
    
    val lookupCfg = node.getInternalValue().accept(this)
    val lookupRegister = this.lastExpressionRegister
    
    val readRegister = nextRegister()
    val attributeNode =
      if (assignFromRegister >= 0)
        new WritePropertyNode(lookupRegister, node.getInternalAttr(), assignFromRegister, "")
      else
        new ReadPropertyNode(lookupRegister, node.getInternalAttr(), readRegister, node.accept(ASTPrettyPrinter))
    this.lastExpressionRegister = if (assignFromRegister >= 0) lastExpressionRegister else readRegister
    
    return lookupCfg.addNode(attributeNode)
                    .connectNodes(lookupCfg.exitNodes, attributeNode)
                    .setExitNode(attributeNode)
  }

  override def visitSubscript(node: Subscript): ControlFlowGraph = {
    return visitSubscript(node, -1)
  }
  
  def visitSubscript(node: Subscript, assignFromRegister: Int): ControlFlowGraph = {
    println("visitSubscript");
    val lastExpressionRegister = this.lastExpressionRegister
    
    println("lookup base")
    val lookupBaseCfg = node.getInternalValue().accept(this)
    val baseRegister = this.lastExpressionRegister
    
    println("lookup property")
    val lookupPropertyCfg = node.getInternalSlice().accept(this)
    val propertyRegister = this.lastExpressionRegister
    
    val readRegister = nextRegister()
    val subscriptNode =
      if (assignFromRegister >= 0)
        new WriteIndexableNode(baseRegister, propertyRegister, assignFromRegister, node.accept(ASTPrettyPrinter))
      else
        new ReadIndexableNode(baseRegister, propertyRegister, readRegister, node.accept(ASTPrettyPrinter))
    this.lastExpressionRegister = if (assignFromRegister >= 0) lastExpressionRegister else readRegister
    
    lookupBaseCfg.combineGraphs(lookupPropertyCfg)
                 .addNode(subscriptNode)
                 .connectNodes(lookupBaseCfg.exitNodes, lookupPropertyCfg.entryNodes)
                 .setEntryNodes(lookupBaseCfg.entryNodes)
                 .connectNodes(lookupPropertyCfg.exitNodes, subscriptNode)
                 .setExitNode(subscriptNode)
  }

  override def visitName(node: Name): ControlFlowGraph = {
    val nameRegister = nextRegister()
    this.lastExpressionRegister = nameRegister
    return ControlFlowGraph.makeSingleton(new ReadVariableNode(node.getInternalId(), nameRegister, node.accept(ASTPrettyPrinter)))
  }

  override def visitList(node: ast.List): ControlFlowGraph = {
    println("visitList");
    return null
  }

  override def visitTuple(node: Tuple): ControlFlowGraph = {
    println("visitTuple");
    
    var registers = List[Int]()
    val valuesCfg = node.getInternalElts().toList.foldLeft(ControlFlowGraph.makeSingleton(new NoOpNode("Tuple entry"))) {(acc, el) =>
      val elCfg = el.accept(this)
      registers = this.lastExpressionRegister :: registers
      
      acc.combineGraphs(elCfg)
         .connectNodes(acc.exitNodes, elCfg.entryNodes)
         .setEntryNodes(acc.entryNodes)
         .setExitNodes(elCfg.exitNodes)
    }
    
    val tupleRegister = nextRegister()
    val tupleNode = new NewTupleNode(tupleRegister, registers.reverse, node.accept(ASTPrettyPrinter))
    this.lastExpressionRegister = tupleRegister
    
    return valuesCfg.addNode(tupleNode)
                    .connectNodes(valuesCfg.exitNodes, tupleNode)
                    .setExitNode(tupleNode)
  }

  override def visitEllipsis(node: Ellipsis): ControlFlowGraph = {
    println("visitEllipsis");
    return null
  }

  override def visitSlice(node: Slice): ControlFlowGraph = {
    println("visitSlice");
    return null
  }

  override def visitExtSlice(node: ExtSlice): ControlFlowGraph = {
    println("visitExtSlice");
    return null
  }

  override def visitIndex(node: Index): ControlFlowGraph = {
    println("visitIndex");
    return null
  }

  override def visitExceptHandler(node: ExceptHandler): ControlFlowGraph = {
    println("visitExceptHandler");
    return null
  }
}