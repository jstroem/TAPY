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

  var loopEntryNode: Node = null
  var loopExitNode: Node = null

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
    return statements.toList.foldLeft(new ControlFlowGraph(entryNode)) {(acc, stm) =>
      val stmCfg = stm.accept(this)
      stmCfg.entryNodes.head match {
        case node: ClassEntryNode => acc.insert(stmCfg).append(new ClassDeclNode(node.classDef.getInternalName()))
        case node: FunctionEntryNode => acc.insert(stmCfg).append(new FunctionDeclNode(node.funcDef.getInternalName()))
        case node: BreakNode => acc.append(stmCfg).setExitNodes(Set()) // !
        case node => acc.append(stmCfg)
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

    val entryNode = new FunctionEntryNode(node.getInternalName(), node)
    val exitNode = new ExitNode(node.getInternalName())

    val bodyCfg = generateCFGOfStatementList(entryNode, node.getInternalBody())
    return bodyCfg.append(exitNode)
  }

  // Note: ClassDeclNode inserted in generateCFGOfStatementList
  override def visitClassDef(node: ClassDef): ControlFlowGraph = {
    println("visitClassDef")
    
    val entryNode = new ClassEntryNode(node.getInternalName(), node)
    val exitNode = new ExitNode(node.getInternalName())

    val bodyCfg = generateCFGOfStatementList(entryNode, node.getInternalBody())
    return bodyCfg.append(exitNode)
  }

  override def visitReturn(node: Return): ControlFlowGraph = {
    val expressionCfg = node.getInternalValue().accept(this)
    val expressionRegister = this.lastExpressionRegister
    
    return expressionCfg.append(new ReturnNode(expressionRegister))
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
          val baseRegister = this.lastExpressionRegister
          
          val lookupPropertyCfg = node.getInternalSlice().accept(this)
          val propertyRegister = this.lastExpressionRegister
          
          lookupBaseCfg.append(lookupPropertyCfg).append(new DelIndexableNode(baseRegister, propertyRegister))
        
        case node: Attribute =>
          val lookupCfg = node.getInternalValue().accept(this)
          val lookupRegister = this.lastExpressionRegister
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
    val tmpVariableRegister = this.lastExpressionRegister
    
    var i = 0
    return targets.foldRight(new ControlFlowGraph(new NoOpNode("Assign entry"))) {(target, acc) =>
      // 1) Generate the CFG for a single target (which may be a tuple or list)
      val targetCfg = target match {
        case t: Name => new ControlFlowGraph(new WriteVariableNode(t.getInternalId(), tmpVariableRegister)) 
        case t: Subscript => visitSubscript(t, tmpVariableRegister)
        case t: Attribute => visitAttribute(t, tmpVariableRegister) 
        case t: Tuple => visitAssignAux(tmpVariableRegister, t.getInternalElts())
        case t: ast.List => visitAssignAux(tmpVariableRegister, t.getInternalElts())
        case t => throw new NotImplementedException()
      }
      i = i + 1
      
      // 2) Combine the accumulator with the generated CFG of the single assignment
      acc.append(targetCfg)
    }
  }
  
  def visitAssignAux(tmpVariableRegister: Int, elts: java.util.List[expr]): ControlFlowGraph = {
    var i = 0
    return elts.toList.foldLeft(new ControlFlowGraph(new NoOpNode("Assign tuple entry"))) {(acc, el) =>
      val indexRegister = nextRegister()
      val indexNode = new ConstantIntNode(indexRegister, new PyInteger(i))
      
      val lookupRegister = nextRegister()
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
    val targetRegister = this.lastExpressionRegister
    
    val valueCfg = node.getInternalValue().accept(this)
    val valueRegister = this.lastExpressionRegister
    
    val writeNode = new BinOpNode(node.getInternalOp(), targetRegister, valueRegister, targetRegister)
    
    return targetCfg.append(valueCfg).append(writeNode)
  }

  override def visitPrint(node: Print): ControlFlowGraph = {
    val exprCfg = if (node.getInternalDest() != null) node.getInternalDest().accept(this) else new ControlFlowGraph(new NoOpNode("No Dest"))
    val destReg = if (node.getInternalDest() != null) Some(lastExpressionRegister) else None
    val valueCfgs = node.getInternalValues().toList.map((a) => {
      val cfg = a.accept(this)
      val reg = lastExpressionRegister
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
    val containerReg = lastExpressionRegister

    val createIterFunctionReg = nextRegister()
    val iterReg = nextRegister()
    val nextObjFunctionReg = nextRegister()
    val nextObjReg = nextRegister()

    lastExpressionRegister = nextObjReg
    val assignCfg = visitAssign(List(node.getInternalTarget()))

    val loopStartNode = new CallNode(nextObjReg, nextObjFunctionReg, List(), Map())
    val exceptNode = new ExceptNode(List("StopIteration"))
    val forExitNode = new NoOpNode("For exit")

    val forOrElseCfg = generateCFGOfStatementList(new NoOpNode("For-or-else entry"), node.getInternalOrelse())

    loopExitNode = forExitNode
    loopEntryNode = loopStartNode

    val forBodyCfg = generateCFGOfStatementList(new NoOpNode("For-body entry"), node.getInternalBody())

    iterCfg = iterCfg.append(new ReadPropertyNode(containerReg, "__iter__", createIterFunctionReg))
                     .append(new CallNode(iterReg, createIterFunctionReg, List(), Map()))
                     .append(new ReadPropertyNode(iterReg, "__next__", nextObjFunctionReg))
                     .append(loopStartNode)
                     .addNode(forExitNode)
                     .append(assignCfg)

    //Make exception state: If there is a StopIteration exception, the exceptNode is reached and afterwards going into the orElseCfg
    iterCfg = iterCfg.addNode(exceptNode)
                     .connectExcept(loopStartNode, exceptNode)
                     .insert(forOrElseCfg, exceptNode, forExitNode)


    //Add the forbody onto the cfg
    return iterCfg.insert(forBodyCfg, Set[Node](), loopStartNode)
  }

  override def visitWhile(node: While): ControlFlowGraph = {
    println("visitWhile")

    val conditionCfg = node.getInternalTest().accept(this)
    val conditionRegister = this.lastExpressionRegister
    
    val whileEntryNode = new IfNode(conditionRegister)
    val whileExitNode = new NoOpNode("While exit")
    
    val oldLoopEntryNode = this.loopEntryNode // In case of nested loops
    val oldLoopExitNode = this.loopExitNode

    val whileBodyCfg = generateCFGOfStatementList(new NoOpNode("While-body entry"), node.getInternalBody())
    val whileOrElseCfg = generateCFGOfStatementList(new NoOpNode("While-or-else entry"), node.getInternalOrelse())
    
    this.loopEntryNode = oldLoopEntryNode // Recover in case of nested loops
    this.loopExitNode = oldLoopExitNode
    println("while now")
    
    return conditionCfg.append(whileEntryNode)
                       .append(whileOrElseCfg)
                       .append(whileExitNode)
                       .insert(whileBodyCfg, whileEntryNode, conditionCfg.entryNodes)
  }

  override def visitIf(node: If): ControlFlowGraph = {
    println("visitIf")
    
    val conditionCfg = node.getInternalTest().accept(this)
    val conditionRegister = this.lastExpressionRegister
    
    val ifEntryNode: IfNode = new IfNode(conditionRegister)
    val ifExitNode = new NoOpNode("If exit")

    // Construct the CFG's for the two branches
    var thenCfg = generateCFGOfStatementList(new NoOpNode("If-then entry"), node.getInternalBody())
    var elseCfg = generateCFGOfStatementList(new NoOpNode("If-else entry"), node.getInternalOrelse())

    return conditionCfg.append(ifEntryNode)
                       .append(Set(thenCfg,
                                   elseCfg))
                       .append(ifExitNode)
  }

  override def visitWith(node: With): ControlFlowGraph = {
    println("visitWith")
    return null
  }

  override def visitRaise(node: Raise): ControlFlowGraph = {
    println("visitRaise")
    val cfg = node.getInternalType().accept(this)
    return cfg.append(new RaiseNode(lastExpressionRegister))
  }

  override def visitTryExcept(node: TryExcept): ControlFlowGraph = {
    println("visitTryExcept")
    return null
  }

  override def visitTryFinally(node: TryFinally): ControlFlowGraph = {
    println("visitTryFinally")
    return null
  }

  override def visitAssert(node: Assert): ControlFlowGraph = {
    println("visitAssert")
    return null
  }

  override def visitImport(node: Import): ControlFlowGraph = {
    println("visitImport")
    return null
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
    val result_reg = nextRegister()
    val ifExitNode = new NoOpNode("If exit")

    val cfgsAndRegisters : List[(ControlFlowGraph, Int)] = exprs.map((a) => {
      val cfg = a.accept(this)
      val reg = lastExpressionRegister
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

    lastExpressionRegister = result_reg

    return foldedCfg.append(otherSideNode).append(ifExitNode)
  }

  override def visitBinOp(node: BinOp): ControlFlowGraph = {
    val leftCfg = node.getInternalLeft().accept(this)
    val leftRegister = lastExpressionRegister
    
    val rightCfg = node.getInternalRight().accept(this)
    val rightRegister = lastExpressionRegister
    
    val resultRegister = nextRegister()
    val binOpNode = new BinOpNode(node.getInternalOp(), leftRegister, rightRegister, resultRegister)

    lastExpressionRegister = resultRegister

    return leftCfg.append(rightCfg).append(binOpNode)
  }

  override def visitUnaryOp(node: UnaryOp): ControlFlowGraph = {
    val cfg = node.getInternalOperand().accept(this)
    val register = this.lastExpressionRegister
    val resultRegister = nextRegister()
    this.lastExpressionRegister = resultRegister
    return cfg.append(new UnOpNode(node.getInternalOp(), register, resultRegister))
  }

  override def visitLambda(node: Lambda): ControlFlowGraph = {
    println("visitLambda")
    return null
  }

  override def visitIfExp(node: IfExp): ControlFlowGraph = {
    println("visitIfExp")
    
    val conditionCfg = node.getInternalTest().accept(this)
    val conditionRegister = this.lastExpressionRegister
    
    val ifNode = new IfNode(conditionRegister)
    val ifExitNode = new NoOpNode("If exit")
    
    val thenCfg = node.getInternalBody().accept(this)
    val thenRegister = this.lastExpressionRegister
    
    val elseCfg = node.getInternalOrelse().accept(this)
    val elseRegister = this.lastExpressionRegister
    
    val resultRegister = nextRegister()
    this.lastExpressionRegister = resultRegister
    
    return conditionCfg.append(ifNode)
                       .append(Set(thenCfg.append(new WriteRegisterNode(resultRegister, thenRegister)),
                                   elseCfg.append(new WriteRegisterNode(resultRegister, elseRegister))))
                       .append(ifExitNode)
  }

  override def visitDict(node: Dict): ControlFlowGraph = {
    println("visitDict")
    
    val emptyDictRegister = nextRegister()
    val emptyDictCfg = new ControlFlowGraph(new NewDictionaryNode(emptyDictRegister))
    
    val dictCfg = node.getInternalKeys().toList.zip(node.getInternalValues().toList).foldLeft(emptyDictCfg) {(acc,entry) =>
      val keyCfg = entry._1.accept(this)
      val keyRegister = this.lastExpressionRegister
      
      val valueCfg = entry._2.accept(this)
      val valueRegister = this.lastExpressionRegister
      
      val writeNode = new WriteIndexableNode(emptyDictRegister, keyRegister, valueRegister)
      
      acc.append(keyCfg).append(valueCfg).append(writeNode)
    }
    this.lastExpressionRegister = emptyDictRegister
    
    return dictCfg
  }

  override def visitSet(node: org.python.antlr.ast.Set): ControlFlowGraph = {
    println("visitSet")
    
    val emptySetRegister = nextRegister()
    val newSetCfg = new ControlFlowGraph(new NewSetNode(emptySetRegister))

    if (node.getInternalElts().size() == 0) {
      // Just return the empty set
      this.lastExpressionRegister = emptySetRegister
      return newSetCfg
      
    } else {
      // Recursively get the CFG parts for the elements
      val pair = node.getInternalElts().toList.map((el) => {
        val elCfg = el.accept(this)
        val elRegister = this.lastExpressionRegister
        (elCfg, elRegister)
      })
  
      this.lastExpressionRegister = emptySetRegister
  
      val setAddFuncRegister = nextRegister()
      val readGetFuncNode = new ReadPropertyNode(emptySetRegister, "add", setAddFuncRegister)

      val newSetAndAddFuncCfg = newSetCfg.append(readGetFuncNode)
      return pair.foldLeft(newSetAndAddFuncCfg)((accCfg, a) => {
        val (elCfg, elRegister) = a
        accCfg.append(elCfg).append(new CallNode(nextRegister(), setAddFuncRegister, List(elRegister), Map()))
      })
    }
  }

  override def visitListComp(node: ListComp): ControlFlowGraph = {
    println("visitListComp")
    return null
  }

  override def visitSetComp(node: SetComp): ControlFlowGraph = {
    println("visitSetComp")
    return null
  }

  override def visitDictComp(node: DictComp): ControlFlowGraph = {
    println("visitDictComp")
    return null
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
    
    val resultRegister = nextRegister()
    val exitNode = new NoOpNode("Compare exit")
    
    var i = 0
    val initialAcc = node.getInternalLeft().accept(this).addNode(exitNode)
    val result = pairs.foldLeft(initialAcc) {(acc, pair) =>
      val (exp, op) = pair
      
      val prevExpRegister = this.lastExpressionRegister
      
      val expCfg = exp.accept(this)
      val expRegister = this.lastExpressionRegister
      
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
    
    this.lastExpressionRegister = resultRegister
    return result.setExitNode(exitNode)
  }
  
  override def visitCall(node: Call): ControlFlowGraph = {
    println("visitCall")
    
    // Lookup the function
    val lookupCfg = node.getInternalFunc().accept(this)
    val lookupRegister = this.lastExpressionRegister
    
    // Lookup the arguments
    var argsRegisters = List[Int]()
    val argsCfg = node.getInternalArgs().toList.foldLeft(new ControlFlowGraph(new NoOpNode("Function-arguments entry"))) {(acc, el) =>
      val elCfg = el.accept(this)
      argsRegisters = this.lastExpressionRegister :: argsRegisters
      
      acc.append(elCfg)
    }
    
    // Call the function with the arguments
    val resultRegister = nextRegister()
    this.lastExpressionRegister = resultRegister
    
    lookupCfg.append(argsCfg).append(new CallNode(resultRegister, lookupRegister, argsRegisters.reverse, Map()))
  }

  override def visitRepr(node: Repr): ControlFlowGraph = {
    println("visitRepr")
    return null
  }

  override def visitNum(node: Num): ControlFlowGraph = {
    val numRegister = nextRegister()
    this.lastExpressionRegister = numRegister
    return node.getInternalN() match {
      case pyInt: PyInteger => new ControlFlowGraph(new ConstantIntNode(numRegister, pyInt))
      case pyLong: PyLong => new ControlFlowGraph(new ConstantLongNode(numRegister, pyLong))
      case pyFloat: PyFloat => new ControlFlowGraph(new ConstantFloatNode(numRegister, pyFloat))
      case pyComplex: PyComplex => new ControlFlowGraph(new ConstantComplexNode(numRegister, pyComplex))
    }
  }

  override def visitStr(node: Str): ControlFlowGraph = {
    val strRegister = nextRegister()
    this.lastExpressionRegister = strRegister
    return new ControlFlowGraph(new ConstantStringNode(strRegister, node.getInternalS().toString()))
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
        new WritePropertyNode(lookupRegister, node.getInternalAttr(), assignFromRegister)
      else
        new ReadPropertyNode(lookupRegister, node.getInternalAttr(), readRegister)
    this.lastExpressionRegister = if (assignFromRegister >= 0) lastExpressionRegister else readRegister
    
    return lookupCfg.append(attributeNode)
  }

  override def visitSubscript(node: Subscript): ControlFlowGraph = {
    return visitSubscript(node, -1)
  }
  
  def visitSubscript(node: Subscript, assignFromRegister: Int): ControlFlowGraph = {
    println("visitSubscript")
    
    val lastExpressionRegister = this.lastExpressionRegister
    
    val lookupBaseCfg = node.getInternalValue().accept(this)
    val baseRegister = this.lastExpressionRegister
    
    val lookupPropertyCfg = node.getInternalSlice().accept(this)
    val propertyRegister = this.lastExpressionRegister
    
    val readRegister = nextRegister()
    val subscriptNode =
      if (assignFromRegister >= 0)
        new WriteIndexableNode(baseRegister, propertyRegister, assignFromRegister)
      else
        new ReadIndexableNode(baseRegister, propertyRegister, readRegister)
    this.lastExpressionRegister = if (assignFromRegister >= 0) lastExpressionRegister else readRegister
    
    lookupBaseCfg.append(lookupPropertyCfg).append(subscriptNode)
  }

  override def visitName(node: Name): ControlFlowGraph = {
    val nameRegister = nextRegister()
    this.lastExpressionRegister = nameRegister
    return new ControlFlowGraph(new ReadVariableNode(node.getInternalId(), nameRegister))
  }

  override def visitList(node: ast.List): ControlFlowGraph = {
    println("visitList")
    val resultReg = nextRegister()
    val newListCfg = new ControlFlowGraph(new NewListNode(resultReg))

    val pair = node.getInternalElts().toList.map((a) => {
      val cfg = a.accept(this)
      val reg = lastExpressionRegister
      (cfg,reg)
    })

    lastExpressionRegister = resultReg

    if (pair.length > 0) {
      val appendFuncReg = nextRegister()
      val readAppendFuncNode = ReadPropertyNode(resultReg, "append", appendFuncReg)

      val newListAndAppendFuncCfg = newListCfg.addNode(readAppendFuncNode).connect(newListCfg.exitNodes, readAppendFuncNode).setExitNode(readAppendFuncNode)
      return pair.foldLeft(newListAndAppendFuncCfg)((accCfg,a) => {
        val (cfg,reg) = a
        val callAppendNode = CallNode(nextRegister(), appendFuncReg, List(reg), Map())
        accCfg.append(cfg).append(callAppendNode)
      })
    } else return newListCfg
  }

  override def visitTuple(node: Tuple): ControlFlowGraph = {
    println("visitTuple")
    
    var registers = List[Int]()
    val valuesCfg = node.getInternalElts().toList.foldLeft(new ControlFlowGraph(new NoOpNode("Tuple entry"))) {(acc, el) =>
      val elCfg = el.accept(this)
      registers = this.lastExpressionRegister :: registers
      
      acc.append(elCfg)
    }
    
    val tupleRegister = nextRegister()
    this.lastExpressionRegister = tupleRegister
    
    return valuesCfg.append(new NewTupleNode(tupleRegister, registers.reverse))
  }

  override def visitEllipsis(node: Ellipsis): ControlFlowGraph = {
    println("visitEllipsis")
    return null
  }

  override def visitSlice(node: Slice): ControlFlowGraph = {
    println("visitSlice")
    return null
  }

  override def visitExtSlice(node: ExtSlice): ControlFlowGraph = {
    println("visitExtSlice")
    return null
  }

  override def visitIndex(node: Index): ControlFlowGraph = {
    println("visitIndex")
    return node.getInternalValue().accept(this)
  }

  override def visitExceptHandler(node: ExceptHandler): ControlFlowGraph = {
    println("visitExceptHandler")
    return null
  }
}