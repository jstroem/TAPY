package tapy

import tapy.cfg._
import org.python.indexer._;
import org.python.indexer.ast._;
import _root_.org.python.antlr._;
import _root_.org.python.antlr.base._;
import _root_.org.python.antlr.runtime._;
import _root_.org.python.antlr.runtime.tree._;

object Main {
  var cfg: ControlFlowGraph = null
  
  def main(args: Array[String]): Unit = {
    val parser: BaseParser = new BaseParser(new ANTLRFileStream("tests/greet.py"), "tests/greet.py", "ascii");
    val ast: mod = parser.parseModule();
    
    println(ast.accept(ASTPrettyPrinter));
    
    cfg = ast.accept(CFGGeneratorVisitor).asInstanceOf[ControlFlowGraph];
    if (cfg != null) {
      println(cfg.prettyPrint());
    } else {
      println("Sorry, no CFG :-(");
    }
  }
}