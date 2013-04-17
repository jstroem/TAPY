package tapy.cfg

import scala.collection.immutable.List
import org.python.antlr.PythonTree
import org.python.antlr.ast._
import org.python.antlr.base._
import scala.collection.JavaConversions._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

object CFGGeneratorVisitor extends VisitorBase[ControlFlowGraph] {
  
  /* Helper methods */
  
  def generateCFGOfStatementList(statements: java.util.List[stmt]): ControlFlowGraph = {
    
    val iterator = statements.iterator()
    var stmsCfg = iterator.next().accept(this)
    while (iterator.hasNext()) {
      val stm = iterator.next()
      val stmCfg = stm.accept(this);
      stmsCfg = stmsCfg.combineGraphs(stmCfg)
                       .setEntryNodes(stmsCfg.entryNodes)
                       .setExitNodes(stmCfg.exitNodes)
                       .connectNodes(stmsCfg.exitNodes, stmCfg.entryNodes)
    }
    return stmsCfg;
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
    return generateCFGOfStatementList(node.getInternalBody())
  }
  
  override def visitInteractive(node: Interactive): ControlFlowGraph = {
    println("visitInteractive")
    return null
  }
  
  override def visitExpression(node: Expression): ControlFlowGraph = {
    println("visitExpression");
    return null;
  }
  
  override def visitSuite(node: Suite): ControlFlowGraph = {
    println("visitSuite");
    return null;
  }
  
  override def visitFunctionDef(node: FunctionDef): ControlFlowGraph = {
    println("visitFunctionDef");
    return null;
  }
  
  override def visitClassDef(node: ClassDef): ControlFlowGraph = {
    println("visitClassDef");
    return null;
  }
  
  override def visitReturn(node: Return): ControlFlowGraph = {
    println("visitReturn");
    return null;
  }
  
  override def visitDelete(node: Delete): ControlFlowGraph = {
    println("visitDelete");
    return null;
  }
  
  override def visitAssign(node: Assign): ControlFlowGraph = {
    println("visitAssign");
    
    var variableNode: Boolean = false
    
    // Find out whether this is a variable or property assignment
    val targets = node.getInternalTargets()
    if (targets.size() == 1) {
      // Single assignment
      val target = targets.get(0)
      target match {
        case t: Name =>
          val cfgNode = new WriteVariableNode(t.getInternalId(), 0, node.accept(ASTPrettyPrinter))
          val cfgNodes = cfgNode :: List()
          return new ControlFlowGraph(cfgNodes, cfgNodes, cfgNodes, Map())
          
        case _ =>
          try {
            target.accept(this)
          } catch {
            case e: Exception =>
          }
          throw new NotImplementedException()
      }
    } else {
      // Multiple assignment
      throw new NotImplementedException()
    }
    return null;
  }
  
  override def visitAugAssign(node: AugAssign): ControlFlowGraph = {
    println("visitAugAssign");
    return null;
  }
  
  override def visitPrint(node: Print): ControlFlowGraph = {
    val printCfgNode = new PrintNode(0, node.accept(ASTPrettyPrinter))
    val cfgNodes = printCfgNode :: List()
    return new ControlFlowGraph(cfgNodes, cfgNodes, cfgNodes, Map())
  }
  
  override def visitFor(node: For): ControlFlowGraph = {
    println("visitFor");
    return null;
  }
  
  override def visitWhile(node: While): ControlFlowGraph = {
    println("visitWhile");
    return null;
  }
  
  override def visitIf(node: If): ControlFlowGraph = {
    println("visitIf")
    
    val thenCfg = generateCFGOfStatementList(node.getInternalBody())
    val elseCfg = generateCFGOfStatementList(node.getInternalOrelse())
    
    // TODO:
    // Are we always sure that there is only 1 entryNode of the two branches?
    // What if the first statement is a function declaration?
    val ifCfgNode = new IfNode(0, thenCfg.entryNodes.get(0), elseCfg.entryNodes.get(0), node.accept(ASTPrettyPrinter))
    val noOpCfgNode = new NoOpNode("")
    
    return thenCfg.combineGraphs(elseCfg)
                  .addNodes(ifCfgNode :: noOpCfgNode :: List())
                  .setEntryNode(ifCfgNode)
                  .setExitNode(noOpCfgNode)
                  .connectNodes(ifCfgNode, thenCfg.entryNodes ++ elseCfg.entryNodes)
                  .connectNodes(thenCfg.exitNodes ++ elseCfg.exitNodes, noOpCfgNode)
    return null
  }
  
  override def visitWith(node: With): ControlFlowGraph = {
    println("visitWith");
    return null;
  }
  
  override def visitRaise(node: Raise): ControlFlowGraph = {
    println("visitRaise");
    return null;
  }
  
  override def visitTryExcept(node: TryExcept): ControlFlowGraph = {
    println("visitTryExcept");
    return null;
  }
  
  override def visitTryFinally(node: TryFinally): ControlFlowGraph = {
    println("visitTryFinally");
    return null;
  }
  
  override def visitAssert(node: Assert): ControlFlowGraph = {
    println("visitAssert");
    return null;
  }
  
  override def visitImport(node: Import): ControlFlowGraph = {
    println("visitImport");
    return null;
  }
  
  override def visitImportFrom(node: ImportFrom): ControlFlowGraph = {
    println("visitImportFrom");
    return null;
  }
  
  override def visitExec(node: Exec): ControlFlowGraph = {
    println("visitExec");
    return null;
  }
  
  override def visitGlobal(node: Global): ControlFlowGraph = {
    println("visitGlobal");
    return null;
  }
  
  override def visitExpr(node: Expr): ControlFlowGraph = {
    println("visitExpr");
    return null;
  }
  
  override def visitPass(node: Pass): ControlFlowGraph = {
    println("visitPass");
    return null;
  }
  
  override def visitBreak(node: Break): ControlFlowGraph = {
    println("visitBreak");
    return null;
  }
  
  override def visitContinue(node: Continue): ControlFlowGraph = {
    println("visitContinue");
    return null;
  }
  
  override def visitBoolOp(node: BoolOp): ControlFlowGraph = {
    println("visitBoolOp");
    return null;
  }
  
  override def visitBinOp(node: BinOp): ControlFlowGraph = {
    println("visitBinOp");
    return null;
  }
  
  override def visitUnaryOp(node: UnaryOp): ControlFlowGraph = {
    println("visitUnaryOp");
    return null;
  }
  
  override def visitLambda(node: Lambda): ControlFlowGraph = {
    println("visitLambda");
    return null;
  }
  
  override def visitIfExp(node: IfExp): ControlFlowGraph = {
    println("visitIfExp");
    return null;
  }
  
  override def visitDict(node: Dict): ControlFlowGraph = {
    println("visitDict");
    return null;
  }
  
  override def visitSet(node: Set): ControlFlowGraph = {
    println("visitSet");
    return null;
  }
  
  override def visitListComp(node: ListComp): ControlFlowGraph = {
    println("visitListComp");
    return null;
  }
  
  override def visitSetComp(node: SetComp): ControlFlowGraph = {
    println("visitSetComp");
    return null;
  }
  
  override def visitDictComp(node: DictComp): ControlFlowGraph = {
    println("visitDictComp");
    return null;
  }
  
  override def visitGeneratorExp(node: GeneratorExp): ControlFlowGraph = {
    println("visitGeneratorExp");
    return null;
  }
  
  override def visitYield(node: Yield): ControlFlowGraph = {
    println("visitYield");
    return null;
  }
  
  override def visitCompare(node: Compare): ControlFlowGraph = {
    println("visitCompare");
    return null;
  }
  
  override def visitCall(node: Call): ControlFlowGraph = {
    println("visitCall");
    return null;
  }
  
  override def visitRepr(node: Repr): ControlFlowGraph = {
    println("visitRepr");
    return null;
  }
  
  override def visitNum(node: Num): ControlFlowGraph = {
    println("visitNum");
    return null;
  }
  
  override def visitStr(node: Str): ControlFlowGraph = {
    println("visitStr");
    return null;
  }
  
  override def visitAttribute(node: Attribute): ControlFlowGraph = {
    println("visitAttribute");
    return null;
  }
  
  override def visitSubscript(node: Subscript): ControlFlowGraph = {
    println("visitSubscript");
    return null;
  }
  
  override def visitName(node: Name): ControlFlowGraph = {
    println("visitName");
    return null;
  }
  
  override def visitList(node: org.python.antlr.ast.List): ControlFlowGraph = {
    println("visitList");
    return null;
  }
  
  override def visitTuple(node: Tuple): ControlFlowGraph = {
    println("visitTuple");
    return null;
  }
  
  override def visitEllipsis(node: Ellipsis): ControlFlowGraph = {
    println("visitEllipsis");
    return null;
  }
  
  override def visitSlice(node: Slice): ControlFlowGraph = {
    println("visitSlice");
    return null;
  }
  
  override def visitExtSlice(node: ExtSlice): ControlFlowGraph = {
    println("visitExtSlice");
    return null;
  }
  
  override def visitIndex(node: Index): ControlFlowGraph = {
    println("visitIndex");
    return null;
  }
  
  override def visitExceptHandler(node: ExceptHandler): ControlFlowGraph = {
    println("visitExceptHandler");
    return null;
  }
}