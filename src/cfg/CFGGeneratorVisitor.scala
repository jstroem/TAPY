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

  /* State */

  var forOrElseCfg: ControlFlowGraph = null
  var whileOrElseCfg: ControlFlowGraph = null
  
  var nextTempVariableIndex = 0
  def nextTempVariable(): String = {
    nextTempVariableIndex = nextTempVariableIndex + 1
    return s"_tmp$nextTempVariableIndex"
  }

  /* Helper methods */

  def printToTest(cfg: ControlFlowGraph): Unit = {
    GraphvizExporter.export(cfg.generateGraphvizGraph(), new PrintStream("test.cfg.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o test.cfg.gif test.cfg.dot")
  }
  
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
    return ControlFlowGraph.makeSingleton(new ReturnNode(0, node.accept(ASTPrettyPrinter)))
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
      // Generate the CFG node of the assignment
      return targets.get(0) match {
        case t: Name =>
          // Single assignment
          ControlFlowGraph.makeSingleton(new WriteVariableNode(t.getInternalId(), 0, node.getInternalValue().accept(ASTPrettyPrinter)))
          
        case t: Subscript =>
          // Single assignment
          ControlFlowGraph.makeSingleton(new WriteDictionaryNode(0, 0, 0, node.accept(ASTPrettyPrinter)))
          
        case t: Attribute =>
          // Single assignment
          ControlFlowGraph.makeSingleton(new WritePropertyNode(0, t.getInternalAttr(), 0, node.accept(ASTPrettyPrinter)))
          
        case t: Tuple => {
          // Multiple assignment
          // Normalize
          //   x_1, ..., x_k = exp
          // into
          //   tmp = exp
          //   x_1 = tmp[0]
          //   ...
          //   x_k = tmp[k]
          val tmpVariableName = nextTempVariable()
          val tmpAssignmentCfgNode = new WriteVariableNode(tmpVariableName, 0, s"${node.getInternalValue().accept(ASTPrettyPrinter)}")
    
          var assignmentCfg = ControlFlowGraph.makeSingleton(tmpAssignmentCfgNode)
    
          val iterator = t.getInternalElts().iterator()
          var i = 0
          var ithMinusOneAssignmentCfgNode: Node = tmpAssignmentCfgNode
          while (iterator.hasNext()) {
            // 1) Make the node for this particular assignment
            val ithAssignmentCfgNode: Node = iterator.next() match {
              case t: Name => new WriteVariableNode(t.getInternalId(), 0, s"$tmpVariableName[$i]")
              case t: Subscript => new WriteDictionaryNode(0, 0, 0, s"${t.accept(ASTPrettyPrinter)} = $tmpVariableName[$i]")
              case t: Attribute => new WritePropertyNode(0, t.getInternalAttr(), 0, s"${t.accept(ASTPrettyPrinter)} = $tmpVariableName[$i]")
              case t: Tuple => {
                null
              }
              case t =>
                try {
                  println("Error")
                  println(t.accept(ASTPrettyPrinter))
                } catch {
                  case e: Exception =>
                }
                throw new NotImplementedException()
            }
    
            // 2) Add it to the multiple assignment cfg
            assignmentCfg = assignmentCfg.addNode(ithAssignmentCfgNode)
              .connectNodes(ithMinusOneAssignmentCfgNode, ithAssignmentCfgNode)
              .setExitNode(ithAssignmentCfgNode)
    
            i = i + 1
            ithMinusOneAssignmentCfgNode = ithAssignmentCfgNode
          }
    
          return assignmentCfg
        }
        
        case _ =>
          try {
          } catch {
            case e: Exception =>
          }
          throw new NotImplementedException()
      }
    } else {
      // Does this even occur? Seems like a "bug" in the AST since multiple assignment is handled by a tuple (see above).
      throw new NotImplementedException()
    }
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
    
    val forEntryCfgNode = new ForInNode(node.accept(ASTPrettyPrinter))
    val forExitCfgNode = new NoOpNode("For exit")
    val forOrElseEntryCfgNode = new NoOpNode("For else")
    
    val oldForOrElseCfg = this.forOrElseCfg // In case of nested for loops
    this.forOrElseCfg = generateCFGOfStatementList(forOrElseEntryCfgNode, node.getInternalOrelse())
    
    val bodyCfg = generateCFGOfStatementList(forEntryCfgNode, node.getInternalBody())
    
    val forOrElseCfg = this.forOrElseCfg
    this.forOrElseCfg = oldForOrElseCfg
    
    if (forOrElseCfg.getNodeSuccessors(forOrElseEntryCfgNode).size() == 0) {
      // There is no or-else branch
      return bodyCfg.addNode(forExitCfgNode)
                    .connectNodes(forEntryCfgNode, forExitCfgNode)
                    .connectNodes(bodyCfg.exitNodes, forEntryCfgNode)
                    .setExitNode(forExitCfgNode)
    } else {
      return bodyCfg.combineGraphs(forOrElseCfg)
                    .addNode(forExitCfgNode)
                    .connectNodes(forEntryCfgNode, forExitCfgNode)
                    .connectNodes(bodyCfg.exitNodes, forEntryCfgNode)
                    .connectNodes(forOrElseCfg.exitNodes, forExitCfgNode)
                    .setEntryNode(forEntryCfgNode)
                    .setExitNode(forExitCfgNode)
    }
  }

  override def visitWhile(node: While): ControlFlowGraph = {
    println("visitWhile");
    val whileEntryCfgNode = new WhileNode(0, s"while ${node.getInternalTest().accept(ASTPrettyPrinter)}: ...")
    val bodyCfg = generateCFGOfStatementList(whileEntryCfgNode, node.getInternalBody())
    return bodyCfg.connectNodes(bodyCfg.exitNodes, whileEntryCfgNode)
                  .setExitNode(whileEntryCfgNode);
  }

  override def visitIf(node: If): ControlFlowGraph = {
    println("visitIf")

    val ifEntryCfgNode: IfNode = new IfNode(0, s"if ${node.getInternalTest().accept(ASTPrettyPrinter)}: ...")
    val ifExitCfgNode = new NoOpNode("If exit")

    // Construct the CFG's for the two branches
    var thenCfg = generateCFGOfStatementList(ifEntryCfgNode, node.getInternalBody())
    var elseCfg = generateCFGOfStatementList(ifEntryCfgNode, node.getInternalOrelse())

    if (elseCfg.getNodeSuccessors(ifEntryCfgNode).size() == 0) {
      // There is no or-else branch
      return thenCfg.addNode(ifExitCfgNode)
                    .connectNodes(thenCfg.exitNodes, ifExitCfgNode)
                    .setExitNode(ifExitCfgNode)
    } else {
      return thenCfg.combineGraphs(elseCfg)
                    .addNode(ifExitCfgNode)
                    .connectNodes(thenCfg.exitNodes ++ elseCfg.exitNodes, ifExitCfgNode)
                    .setEntryNode(ifEntryCfgNode)
                    .setExitNode(ifExitCfgNode)
    }
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
    val breakCfgNode = NoOpNode("Break")
    if (forOrElseCfg != null) {
      // Notice: The break CFG node is not actually added to the forOrElseCfg
      // until it is combined with the bodyCfg in the visitFor-method
      forOrElseCfg = forOrElseCfg.connectNodes(breakCfgNode, forOrElseCfg.entryNodes)
      return ControlFlowGraph.makeSingleton(breakCfgNode)
    } else if (whileOrElseCfg != null) {
      // Notice: The break CFG node is not actually added to the whileOrElseCfg
      // until it is combined with the bodyCfg in the visitWhile-method
      whileOrElseCfg = whileOrElseCfg.connectNodes(breakCfgNode, whileOrElseCfg.entryNodes)
      return ControlFlowGraph.makeSingleton(breakCfgNode)
    }
    throw new InternalError("Break statement outside for or while loop.")
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