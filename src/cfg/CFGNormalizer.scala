package tapy.cfg

import tapy.constants._

object CFGNormalizer {
	def normalize(cfg : ControlFlowGraph): ControlFlowGraph = {
		println("test");
		cfg.nodes.foldLeft(cfg)((cfg,node) => node match {
			case node: ReadIndexableNode => handleReadIndexableNode(node, cfg)
			case node: WriteIndexableNode => handleWriteIndexableNode(node, cfg)
			case node: ReadPropertyNode => handleReadPropertyNode(node, cfg)
			case _ => cfg
		})
	}

	def handleReadIndexableNode(node : ReadIndexableNode, cfg: ControlFlowGraph) : ControlFlowGraph = {
		/** 
		ReadIndaxableNode unrolling to:				<<ifCfg>>
		<1> = HasAttribute(baseReg, "__getitem__")
		if <1>:										<<truePathCfg>>
			Assert(<1>)
			<2> = ReadProperty(baseReg, "__getitem__")
			<res> = Call(<3>, List(propertyReg))
		else:										<<falsePathCfg>>
			AssertNot(<1>)
			<3> = ReadProperty(BUILT_IN, "TypeError")
			<4> = CallNode(<3>, List())
			Raise <4>
		**/
		val hasGetItemReg = Registers.next()
		var ifCfg = new ControlFlowGraph(HasAttributeNode(node.baseReg, "__getitem__", hasGetItemReg))
								 .append(IfNode(hasGetItemReg))

		val getItemReg = Registers.next()
		val truePathCfg = new ControlFlowGraph(AssertNode(hasGetItemReg))
									   .append(ReadPropertyNode(node.baseReg, "__getitem__", getItemReg))
									   .append(CallNode(getItemReg, List(node.propertyReg)))
									   .append(AfterCallNode(node.resultReg))


		val typeErrorRefReg = Registers.next() 
		val typeErrorObjReg = Registers.next()
		val falsePathCfg = new ControlFlowGraph(AssertNode(hasGetItemReg,true))
										.append(ReadPropertyNode(StackConstants.BUILTIN_MODULE, "TypeError", typeErrorRefReg))
										.append(CallNode(typeErrorRefReg, List()))
										.append(AfterCallNode(typeErrorObjReg))
										.append(RaiseNode(Some(typeErrorObjReg)))

		return cfg.replace(node, ifCfg.append(Set(truePathCfg,falsePathCfg)))
	}

	def handleWriteIndexableNode(node : WriteIndexableNode, cfg: ControlFlowGraph) : ControlFlowGraph = {
		/** 
		ReadIndaxableNode unrolling to:				<<ifCfg>>
		<1> = HasAttribute(baseReg, "__setitem__")
		if <1>:										<<truePathCfg>>
			Assert(<1>)
			<2> = ReadProperty(baseReg, "__setitem__")
			<res> = Call(<3>, List(propertyReg,valueReg))
		else:										<<falsePathCfg>>
			AssertNot(<1>)
			<3> = ReadProperty(BUILT_IN, "TypeError")
			<4> = CallNode(<3>, List())
			Raise <4>
		**/
		val hasSetItemReg = Registers.next()
		var ifCfg = new ControlFlowGraph(HasAttributeNode(node.baseReg, "__setitem__", hasSetItemReg))
								 .append(IfNode(hasSetItemReg))

		val setItemReg = Registers.next()
		val truePathCfg = new ControlFlowGraph(AssertNode(hasSetItemReg))
									   .append(ReadPropertyNode(node.baseReg, "__setitem__", setItemReg))
									   .append(CallNode(setItemReg, List(node.propertyReg,node.valueReg)))
									   .append(AfterCallNode(Registers.next()))


		val typeErrorRefReg = Registers.next() 
		val typeErrorObjReg = Registers.next()
		val falsePathCfg = new ControlFlowGraph(AssertNode(hasSetItemReg,true))
										.append(ReadPropertyNode(StackConstants.BUILTIN_MODULE, "TypeError", typeErrorRefReg))
										.append(CallNode(typeErrorRefReg, List()))
										.append(AfterCallNode(typeErrorObjReg))
										.append(RaiseNode(Some(typeErrorObjReg)))

		return cfg.replace(node, ifCfg.append(Set(truePathCfg,falsePathCfg)))
	}

	def handleReadPropertyNode(node: ReadPropertyNode, cfg: ControlFlowGraph) : ControlFlowGraph = {
		/** 
		ReadPropertyNode unrolling to: 				<<ifPropertyCfg>>

		<1> = HasAttribute(baseReg, property)
		If <1>:										<<ifPropertyExistCfg>>
			Assert(<1>)
			<res> = ReadProperty(baseReg, property)
		else:										<<ifPropertyNotExistCfg>>
			AssertNot(<1>)
			<2> = HasAttribute(baseReg, "__getattr__")
			if <2>:									<<ifGetAttrExistCfg>>
				assert(<2>)
				<3> = ReadProperty(baseReg, "__getattr__")
				<4> = StringConstant(property)
				<res> = Call(<3>, [<4>])
			else:									<<ifGetAttrNotExistCfg>>
				assertNot(<2>)
				<5> = ReadProperty(BUILT_IN, "AttributeError")
				<6> = Call(<5>,[])
				Raise <6>
		**/
		var hasPropertyReg = Registers.next()
		val ifPropertyCfg = new ControlFlowGraph(HasAttributeNode(node.baseReg, node.property, hasPropertyReg))
										 .append(IfNode(hasPropertyReg))


		val ifPropertyExistCfg = new ControlFlowGraph(AssertNode(hasPropertyReg))
		                                      .append(ReadPropertyNode(node.baseReg, node.property, node.resultReg))

		val hasGetAttrReg = Registers.next()
		val ifPropertyNotExistCfg = new ControlFlowGraph(AssertNode(hasPropertyReg,true))
												 .append(HasAttributeNode(node.baseReg, "__getattr__", hasGetAttrReg))
												 .append(IfNode(hasGetAttrReg))


		val getAttrReg = Registers.next()
		val propertyValueReg = Registers.next() 
		val ifGetAttrExistCfg = new ControlFlowGraph(AssertNode(hasGetAttrReg))
											 .append(ReadPropertyNode(node.baseReg, "__getattr__", getAttrReg))
											 .append(ConstantStringNode(propertyValueReg,node.property))
											 .append(CallNode(getAttrReg,List(propertyValueReg)))
											 .append(AfterCallNode(node.resultReg))

		val attributeErrorReg = Registers.next()
		val attributeErrorObjReg = Registers.next()
		val ifGetAttrNotExistCfg = new ControlFlowGraph(AssertNode(hasGetAttrReg,true))
												.append(ReadPropertyNode(StackConstants.BUILTIN_MODULE, "AttributeError", attributeErrorReg))
												.append(CallNode(attributeErrorReg,List()))
												.append(AfterCallNode(attributeErrorObjReg))
												.append(RaiseNode(Some(attributeErrorObjReg)))

		return cfg.replace(node, ifPropertyCfg.append(Set(ifPropertyExistCfg, 
														  ifPropertyNotExistCfg.append(Set(ifGetAttrExistCfg, 
														  								   ifGetAttrNotExistCfg)))))
	}

	

}