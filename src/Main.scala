package tapy

import _root_.org.python.antlr._;
import _root_.org.python.antlr.base._;
import _root_.org.python.antlr.runtime._;
import _root_.org.python.antlr.runtime.tree._;
import tapy.cfg._

object Main {
  var node: mod = null
  var cfg: ControlFlowGraph = null
  
  def main(args: Array[String]): Unit = {
    
    val parser: BaseParser = new BaseParser(new ANTLRFileStream("tests/greet.py"), "tests/greet.py", "ascii");
    node = parser.parseModule()
    
    cfg = node.accept(CFGGeneratorVisitor);
    
    println(node.toStringTree());
  }
}