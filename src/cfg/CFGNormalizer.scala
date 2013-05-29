package tapy.cfg

import tapy.constants._

object CFGNormalizer {
	def normalize(cfg : ControlFlowGraph): ControlFlowGraph = {
		println("test");
		cfg.nodes.foldLeft(cfg)((cfg,node) => node match {
			case node: ReadIndexableNode => handleIndexableNode(node, cfg)
			case _ => cfg
		})
	}

	def handleIndexableNode(node : ReadIndexableNode, cfg: ControlFlowGraph) : ControlFlowGraph = {
		val hasGetItemReg = Registers.next()
		val ifNode = IfNode(hasGetItemReg)
		var ifCfg = new ControlFlowGraph(HasAttributeNode(node.baseReg, "__getitem__", hasGetItemReg)).append(ifNode)

		val getItemReg = Registers.next()
		val truePathCfg = new ControlFlowGraph(AssertNode(hasGetItemReg)).append(ReadPropertyNode(node.baseReg, "__getitem__", getItemReg))
																	     .append(CallNode(getItemReg, List(node.propertyReg)))
																	     .append(AfterCallNode(node.resultReg))


		val typeErrorRefReg = Registers.next() 
		val typeErrorObjReg = Registers.next()
		val falsePathCfg = new ControlFlowGraph(AssertNode(hasGetItemReg,true)).append(ReadPropertyNode(StackConstants.BUILTIN_MODULE, "TypeError", typeErrorRefReg))
																			   .append(CallNode(typeErrorRefReg, List()))
																			   .append(AfterCallNode(typeErrorObjReg))
																			   .append(RaiseNode(Some(typeErrorObjReg)))

		
		return cfg.replace(node, ifCfg.append(Set(truePathCfg,falsePathCfg)))
	}
}