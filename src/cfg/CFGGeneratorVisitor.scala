package tapy.cfg

import scala.collection.immutable.List
import tapy.export._
import org.python.antlr.PythonTree
import org.python.antlr.ast._
import org.python.antlr.base._
import scala.collection.JavaConversions._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import java.io._

object CFGGeneratorVisitor extends VisitorBase[ControlFlowGraph] {
  
  /* Helper methods */
  
  def generateCFGOfStatementList(entryNode: Node, statements: java.util.List[stmt]): ControlFlowGraph = {
    var stmsCfg = ControlFlowGraph.makeSingleton(entryNode)
    
    val iterator = statements.iterator()
    while (iterator.hasNext()) {
      val stmCfg = iterator.next().accept(this);
      stmCfg.entryNodes.get(0) match {
        case node: EntryNode =>
          stmsCfg = stmsCfg.combineGraphs(stmCfg)
                           .setEntryNodes(stmsCfg.entryNodes)
                           .setExitNodes(stmsCfg.exitNodes)
        case node =>
          stmsCfg = stmsCfg.combineGraphs(stmCfg)
                           .setEntryNodes(stmsCfg.entryNodes)
                           .setExitNodes(stmCfg.exitNodes)
                           .connectNodes(stmsCfg.exitNodes, stmCfg.entryNodes)
      }
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
    println("visitModule")
    return generateCFGOfStatementList(new NoOpNode("Program entry"), node.getInternalBody())
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
    println("visitFunctionDef")
    
    val entryCfgNode = new EntryNode(node.getInternalName())
    val exitCfgNode = new ExitNode(node.getInternalName())
    
    val bodyCfg = generateCFGOfStatementList(entryCfgNode, node.getInternalBody())
    
    // No need to add entryCfgNode, as this has already been added to bodyCfg (and set to entry node)
    return bodyCfg.addNodes(exitCfgNode :: List())
                  .setExitNode(exitCfgNode)
                  .connectNodes(bodyCfg.exitNodes, exitCfgNode)
  }
  
  override def visitClassDef(node: ClassDef): ControlFlowGraph = {
    println("visitClassDef")
    val entryCfgNode = new EntryNode(node.getInternalName())
    val exitCfgNode = new ExitNode(node.getInternalName())
    
    val bodyCfg = generateCFGOfStatementList(entryCfgNode, node.getInternalBody())
    
    // No need to add entryCfgNode, as this has already been added to bodyCfg (and set to entry node)
    return bodyCfg.addNodes(exitCfgNode :: List())
                  .setExitNode(exitCfgNode)
                  .connectNodes(bodyCfg.exitNodes, exitCfgNode)
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
      
      // Generate the CFG node of the assignment
      val cfgNode: Node = targets.get(0) match {
        case t: Name => new WriteVariableNode(t.getInternalId(), 0, node.accept(ASTPrettyPrinter))
        case t: Subscript => new WriteDictionaryNode(0, 0, 0, node.accept(ASTPrettyPrinter))
        case t: Attribute => new WritePropertyNode(0, t.getInternalAttr(), 0, node.accept(ASTPrettyPrinter))
        case _ =>
          try {
            println(node.accept(ASTPrettyPrinter))
          } catch {
            case e: Exception =>
          }
          throw new NotImplementedException()
      }
      
      return ControlFlowGraph.makeSingleton(cfgNode)
    } else {
      // Multiple assignment
      throw new NotImplementedException()
    }
    return null;
  }
  
  override def visitAugAssign(node: AugAssign): ControlFlowGraph = {
    println("visitAugAssign");
    // TODO
    return null;
  }
  
  override def visitPrint(node: Print): ControlFlowGraph = {
    return ControlFlowGraph.makeSingleton(new PrintNode(0, node.accept(ASTPrettyPrinter)))
  }
  
  override def visitFor(node: For): ControlFlowGraph = {
    println("visitFor");
    // TODO
    return null;
  }
  
  override def visitWhile(node: While): ControlFlowGraph = {
    println("visitWhile");
    // TODO
    return null;
  }
  
  override def visitIf(node: If): ControlFlowGraph = {
    println("visitIf")
    
    val noOpNode = new NoOpNode("")
    
    // Construct the CFG's for the two branches
    var thenCfg = generateCFGOfStatementList(noOpNode, node.getInternalBody())
    var elseCfg = generateCFGOfStatementList(noOpNode, node.getInternalOrelse())
    
    // Remove the no operation node from both CFG's
    thenCfg = thenCfg.removeNode(noOpNode).setEntryNodes(thenCfg.getNodeSuccessors(noOpNode))
    elseCfg = elseCfg.removeNode(noOpNode).setEntryNodes(elseCfg.getNodeSuccessors(noOpNode))
    
    val ifEntryCfgNode: IfNode = new IfNode(0, thenCfg.entryNodes.get(0), elseCfg.entryNodes.get(0), node.accept(ASTPrettyPrinter))
    val ifExitCfgNode = new NoOpNode("")
    
    return thenCfg.combineGraphs(elseCfg)
                  .addNodes(ifEntryCfgNode :: ifExitCfgNode :: List())
                  .setEntryNode(ifEntryCfgNode)
                  .setExitNode(ifExitCfgNode)
                  .connectNodes(ifEntryCfgNode, thenCfg.entryNodes ++ elseCfg.entryNodes)
                  .connectNodes(thenCfg.exitNodes ++ elseCfg.exitNodes, ifExitCfgNode)
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
    return node.getInternalValue().accept(this)
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
    return ControlFlowGraph.makeSingleton(new CallNode(0, None, 0, List(), node.accept(ASTPrettyPrinter)))
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