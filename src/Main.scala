package tapy

import tapy.cfg._
import org.python.indexer._
import org.python.indexer.ast._
import org.python.antlr._
import org.python.antlr.base._
import org.python.antlr.runtime._
import org.python.antlr.runtime.tree._
import java.io._
import tapy.export._
import tapy.dfa._
import tapy.lattices._
import tapy.typeanalysis.TypeAnalysis
import tapy.lattices.HeapLattice
import tapy.cfg.ReadVariableNode

object Main {
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

    try {
      val ast = parser.parseModule()
      println("\n----------\n")
      println("Pretty printing AST of \"" + file + "\"\n")
      val aspp = ast.accept(ASTPrettyPrinter)
      new PrintStream(dir+fname+".ast").print(aspp)
  
      println("\n----------\n")
      println("Generation CFG of \"" + file + "\"\n")
      var now = System.currentTimeMillis();
      var cfgMin = ast.accept(new CFGGeneratorVisitor("__main__")).minify().normalize()
      cfgMin = if (cfgMin.exitNodes.size == 1) cfgMin else cfgMin.append(NoOpNode("Module Exit"))
      println("...done in " + (System.currentTimeMillis() - now) + " ms")
      
      println("\n----------\n")
      println("Generation analysis result of \"" + file + "\"\n")
      now = System.currentTimeMillis();
      val worklist = new Worklist[AnalysisLattice.Elt](new TypeAnalysis(cfgMin), AnalysisLattice, cfgMin, dir)
      val solution = worklist.run()
      println("...done in " + (System.currentTimeMillis() - now) + " ms")
  
      println("\n----------\n")
      println("Pretty printing CFG of \"" + file + "\"\n")
      now = System.currentTimeMillis();
      cfgMin.exportToFile(dir + fname, true, false, AnalysisLattice.getCallGraph(solution))
      println("...done in " + (System.currentTimeMillis() - now) + " ms")
      
      println("\n----------\n")
      println("Pretty printing analysis result of \"" + file + "\"\n")
      now = System.currentTimeMillis();
      new PrintStream(dir+fname+".res.txt").print(AnalysisLattice.eltToString(solution, ""))
      println("...done in " + (System.currentTimeMillis() - now) + " ms")

      if (cfgMin.exitNodes.size > 0) {
        println("\n----------\n")
        println("Pretty printing heap for the CFG exit node of \"" + file + "\"\n")
        now = System.currentTimeMillis();
        HeapLattice.exportToFile(AnalysisLattice.getHeap(cfgMin.exitNodes.head, solution), dir + fname)
        println("...done in " + (System.currentTimeMillis() - now) + " ms")
      }
      
      AnalysisLattice.getProgramState(solution) match {
        case ProgramStateLattice.Concrete(map) =>
          map.foreach{(entry) =>
            val (node, state) = entry
            node match {
              case node: ReadVariableNode =>
                if (node.variable.startsWith("__Analysis_Dump_")) {
                  val name = node.variable.replaceFirst("__Analysis_Dump_", "").replace("__", "")
                  
                  println("\n----------\n")
                  println("Pretty printing heap for dump " + name + " of \"" + file + "\"\n")
                  now = System.currentTimeMillis();
                  HeapLattice.exportToFile(StateLattice.getHeap(state), dir + fname + "-dump-" + name)
                  println("...done in " + (System.currentTimeMillis() - now) + " ms")
                }
                
              case _ =>
            }
          }
          
        case _ =>
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def stringToFile(str: String, file: String): Unit = {
    val out = new PrintStream(file)
    out.print(str)
    out.flush()
    out.close()
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
