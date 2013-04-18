package tapy

import tapy.cfg._
import org.python.indexer._;
import org.python.indexer.ast._;
import org.python.antlr._;
import org.python.antlr.base._;
import org.python.antlr.runtime._;
import org.python.antlr.runtime.tree._;
import java.io._
import tapy.export._

object Main {
  var cfg: ControlFlowGraph = null

  def main(args: Array[String]): Unit = {
    args.foreach((f) => {
      var file = new File(f)
      if (file.isFile()){
        analyzeFile(file)
      } else if (file.isDirectory()) {
        analyzeDirectory(file)
      } else {
        System.err.println("Couldn't open: " + f)
      }
    })
  }

  def analyzeFile(file: File) : Unit = {
    val parser: BaseParser = new BaseParser(new ANTLRFileStream(file.getPath()), file.getPath(), "ascii")
    var (dir,fname,fext) = splitFilename(file)

    val ast = parser.parseModule()
    println("\n----------\n")
    println("Pretty printing AST of \"" + file + "\"\n")
    println("\n" + ast.accept(ASTPrettyPrinter))

    println("\n----------\n")
    println("Generating CFG of \"" + file + "\"\n")
    cfg = ast.accept(CFGGeneratorVisitor)
    
    println("\n----------\n")
    println("Pretty printing CFG of \"" + file + "\"\n")
    val graphvizGraph = cfg.generateGraphvizGraph()

    GraphvizExporter.export(graphvizGraph, new PrintStream(dir + fname+".cfg.dot"))
    Runtime.getRuntime().exec("dot -Tgif -o "+dir + fname+".cfg.gif " + dir + fname+".cfg.dot")
  }

  def analyzeDirectory(dir : File) : Unit = {
    dir.listFiles().foreach((file) => {
      if (file.isFile()){
        analyzeFile(file)
      } else if (file.isDirectory()) {
        analyzeDirectory(file)
      }
    })
  }

  def splitFilename(f : File) : (String,String,String) = {
    var dir = f.getParent() + "/"
    var file = f.getName()
    var idx = file.lastIndexOf('.')
    var extension = if (idx >= 0)  file.substring(idx) else ""
    var filename = if (idx >= 0) file.substring(0, idx) else file
    (dir,filename,extension)
  }
}
