package org.erlide.engine.new_model.internal

import java.util.Map
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.SourceElementBody
import org.eclipse.handly.model.impl.StructureHelper

class ErlFileStructureBuilder extends StructureHelper {

	val Map<IHandle, Body> newElements
	val ErlangAST ast

	new(Map<IHandle, Body> newElements, ErlangAST ast) {
		super(newElements)
		this.newElements = newElements
		this.ast = ast
	}

	def buildStructure(ErlSource source, SourceElementBody parentBody) {

		for (node : ast.forms) {
			val handle = switch node.kind {
				case "attribute":
					new ErlAttribute(source, node.name)
				case "function":
					new ErlFunction(source, node.name, node.arg)
			}
			val body = new SourceElementBody()
			body.fullRange = node.pos
			addChild(parentBody, handle, body)
			complete(body)
		}

		complete(parentBody)
	}
}
