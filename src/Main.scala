package tapy

import _root_.org.python.antlr._;
import _root_.org.python.antlr.base._;
import _root_.org.python.antlr.runtime._;
import _root_.org.python.antlr.runtime.tree._;
import tapy.cfg._
import java.io._

import tapy.export._

object Main {
	var node: mod = null
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
		val parser: BaseParser = new BaseParser(new ANTLRFileStream(file.getPath()), file.getPath(), "ascii");
		var (dir,fname,fext) = splitFilename(file)

		node = parser.parseModule()
		cfg = node.accept(CFGGeneratorVisitor);

		val graphvizGraph = cfg.generateGraphvizGraph()

		GraphvizExporter.export(graphvizGraph, new PrintStream(dir + fname+".cfg.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + fname+".cfg.gif " + dir + fname+".cfg.dot")

		println(node.toStringTree());
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