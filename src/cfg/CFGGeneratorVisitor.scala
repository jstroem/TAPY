package tapy.cfg

import scala.collection.immutable.Set
import tapy.constants
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

  var forExitCfgNode: Node = null
  var whileExitCfgNode: Node = null

  var nextTempVariableIndex = 0
  def nextTempVariable(): String = {
    nextTempVariableIndex = nextTempVariableIndex + 1
    return s"_tmp$nextTempVariableIndex"
  }

  /* Helper methods */

  def generateCFGOfStatementList(entryNode: Node, statements: java.util.List[stmt]): ControlFlowGraph = {
    var stmsCfg = ControlFlowGraph.makeSingleton(entryNode)

    val iterator = statements.iterator()
    while (iterator.hasNext()) {
      val stmCfg = iterator.next().accept(this);
      stmCfg.entryNodes.head match {
        case node: EntryNode =>
          stmsCfg = stmsCfg.combineGraphs(stmCfg)
            .setEntryNodes(stmsCfg.entryNodes)
            .setExitNodes(stmsCfg.exitNodes)
        case node: BreakNode =>
          return stmsCfg.combineGraphs(stmCfg)
            .setEntryNodes(stmsCfg.entryNodes)
            .setExitNodes(Set()) // !
            .connectNodes(stmsCfg.exitNodes, stmCfg.entryNodes)
        case node =>
          stmsCfg = stmsCfg.combineGraphs(stmCfg)
            .setEntryNodes(stmsCfg.entryNodes)
            .setExitNodes(stmCfg.exitNodes)
            .connectNodes(stmsCfg.exitNodes, stmCfg.entryNodes)
      }
    }
    return stmsCfg;
  }

  def operatorTypeToBinop(operator: operatorType): constants.BinOp.Value = operator match {
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

    // Normalize
    //   x_1 = ... = x_k = exp
    // into
    //   x_k = exp
    //   ...
    //   x_1 = x_2
    var oldTarget: expr = null
    var j = 0
    return node.getInternalTargets().toList.foldRight(ControlFlowGraph.makeSingleton(new NoOpNode("Assignment entry"))) {(target, acc) =>
      // 1) Generate the CFG of a single assignment (which may be a tuple)
      val targetCfg = target match {
        case t: Name =>
          // Single assignment
          if (oldTarget == null)
            ControlFlowGraph.makeSingleton(new WriteVariableNode(t.getInternalId(), 0, node.getInternalValue().accept(ASTPrettyPrinter)))
          else
            ControlFlowGraph.makeSingleton(new WriteVariableNode(t.getInternalId(), 0, oldTarget.accept(ASTPrettyPrinter)))

        case t: Subscript =>
          // Single assignment
          if (oldTarget == null)
            ControlFlowGraph.makeSingleton(new WriteDictionaryNode(0, 0, 0, node.accept(ASTPrettyPrinter)))
          else
            ControlFlowGraph.makeSingleton(new WriteDictionaryNode(0, 0, 0, oldTarget.accept(ASTPrettyPrinter)))

        case t: Attribute =>
          // Single assignment
          if (oldTarget == null)
            ControlFlowGraph.makeSingleton(new WritePropertyNode(0, t.getInternalAttr(), 0, node.accept(ASTPrettyPrinter)))
          else
            ControlFlowGraph.makeSingleton(new WritePropertyNode(0, t.getInternalAttr(), 0, oldTarget.accept(ASTPrettyPrinter)))

        case t: Tuple => {
          // Multiple assignment
          // Normalize
          //   x_1, ..., x_k = exp
          // into
          //   tmp = exp
          //   x_1 = tmp[0]
          //   ...
          //   x_k = tmp[k]
          
          val (tmpVariableName: String, tmpAssignmentCfg: ControlFlowGraph) =
            if (oldTarget == null) {
              // This is x_k, so we need to create a temporary variable for exp
              val tmpVariableName = nextTempVariable()
              (tmpVariableName, ControlFlowGraph.makeSingleton(new WriteVariableNode(tmpVariableName, 0, s"${node.getInternalValue().accept(ASTPrettyPrinter)}")))
            } else
              // This is x_i for some i < k, so don't create a temporary variable
              (oldTarget.accept(ASTPrettyPrinter), acc)
  
          var assignmentCfg = tmpAssignmentCfg
  
          val iterator = t.getInternalElts().iterator()
          var i = 0
          var ithMinusOneAssignmentCfg = tmpAssignmentCfg
          while (iterator.hasNext()) {
            // A) Make the node for this particular assignment
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
            val ithAssignmentCfg = ControlFlowGraph.makeSingleton(ithAssignmentCfgNode)
  
            // B) Add it to the multiple assignment CFG
            assignmentCfg = assignmentCfg.combineGraphs(ithAssignmentCfg)
                                         .connectNodes(ithMinusOneAssignmentCfg.exitNodes, ithAssignmentCfg.entryNodes)
                                         .setEntryNodes(assignmentCfg.entryNodes)
                                         .setExitNodes(ithAssignmentCfg.exitNodes)
            assignmentCfg.exportToFile("iteration-" + j + "-" + i)
            
            i = i + 1
            ithMinusOneAssignmentCfg = ithAssignmentCfg
          }

          assignmentCfg
        }
  
        case t =>
          try {
          } catch {
            case e: Exception =>
          }
          throw new NotImplementedException()
      }
      
      // 2) Update oldTarget and combine the accumulator with the generated CFG of the single assignment
      oldTarget = target
      
      j = j + 1
      
      target match {
        case t: Tuple =>
          // Accumulator has already been connected to targetCfg (!)
          println("RETURN TUPLE")
          acc.combineGraphs(targetCfg)
             .setEntryNodes(acc.entryNodes)
             .setExitNodes(targetCfg.exitNodes).exportToFile("iteration-" + j)
        case t =>
          acc.combineGraphs(targetCfg)
             .connectNodes(acc.exitNodes, targetCfg.entryNodes)
             .setEntryNodes(acc.entryNodes)
             .setExitNodes(targetCfg.exitNodes).exportToFile("iteration-" + j)
      }
    }
  }

  override def visitAugAssign(node: AugAssign): ControlFlowGraph = {
    println("visitAugAssign");
    val op = operatorTypeToBinop(node.getInternalOp())
    return ControlFlowGraph.makeSingleton(new BinOpNode(op, 0, 0, 0, node.accept(ASTPrettyPrinter)))
  }

  override def visitPrint(node: Print): ControlFlowGraph = {
    return ControlFlowGraph.makeSingleton(new PrintNode(0, node.accept(ASTPrettyPrinter)))
  }

  override def visitFor(node: For): ControlFlowGraph = {
    println("visitFor");

    val forEntryCfgNode = new ForInNode(node.accept(ASTPrettyPrinter))
    val forOrElseEntryCfgNode = new NoOpNode("For else")

    val oldForExitCfgNode = this.forExitCfgNode // In case of nested loops
    val oldWhileExitCfgNode = this.whileExitCfgNode // In case of nested loops

    this.forExitCfgNode = new NoOpNode("For exit")
    this.whileExitCfgNode = null

    val forBodyCfg = generateCFGOfStatementList(forEntryCfgNode, node.getInternalBody())
    val forOrElseCfg = generateCFGOfStatementList(forOrElseEntryCfgNode, node.getInternalOrelse())

    val forExitCfgNode = this.forExitCfgNode
    this.forExitCfgNode = oldForExitCfgNode // Recover in case of nested loops
    this.whileExitCfgNode = oldWhileExitCfgNode // Recover in case of nested loops

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
    val whileOrElseEntryCfgNode = new NoOpNode("While else")

    val oldForExitCfgNode = this.forExitCfgNode // In case of nested loops
    val oldWhileExitCfgNode = this.whileExitCfgNode // In case of nested loops

    this.forExitCfgNode = null
    this.whileExitCfgNode = new NoOpNode("While exit")

    val whileBodyCfg = generateCFGOfStatementList(whileEntryCfgNode, node.getInternalBody())
    val whileOrElseCfg = generateCFGOfStatementList(whileOrElseEntryCfgNode, node.getInternalOrelse())

    val whileExitCfgNode = this.whileExitCfgNode
    this.forExitCfgNode = oldForExitCfgNode // Recover in case of nested loops
    this.whileExitCfgNode = oldWhileExitCfgNode // Recover in case of nested loops

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

    val ifEntryCfgNode: IfNode = new IfNode(0, s"if ${node.getInternalTest().accept(ASTPrettyPrinter)}: ...")
    val ifExitCfgNode = new NoOpNode("If exit")

    // Construct the CFG's for the two branches
    var thenCfg = generateCFGOfStatementList(ifEntryCfgNode, node.getInternalBody())
    var elseCfg = generateCFGOfStatementList(ifEntryCfgNode, node.getInternalOrelse())

    if (elseCfg.getNodeSuccessors(ifEntryCfgNode).size() == 0) {
      // There is no or-else branch
      return thenCfg.addNode(ifExitCfgNode)
        .connectNodes(ifEntryCfgNode, ifExitCfgNode)
        .connectNodes(thenCfg.exitNodes, ifExitCfgNode)
        .setExitNode(ifExitCfgNode)
    } else {
      // TODO: Break node must not go to the IfExitNode in ex. 10
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
    println("visitPass")
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

  override def visitSet(node: org.python.antlr.ast.Set): ControlFlowGraph = {
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