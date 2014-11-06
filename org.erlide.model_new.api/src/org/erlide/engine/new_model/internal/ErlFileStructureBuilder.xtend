package org.erlide.engine.new_model.internal

import java.util.Map
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.SourceElementBody
import org.eclipse.handly.model.impl.StructureHelper
import org.eclipse.handly.util.TextRange

class ErlFileStructureBuilder extends StructureHelper {

	val Map<IHandle, Body> newElements
	val ErlangAST ast

	new(Map<IHandle, Body> newElements, ErlangAST ast) {
		super(newElements)
		this.newElements = newElements
		this.ast = ast
	}

	def buildStructure(ErlSource source, SourceElementBody parentBody) {

		// TODO fix me
		val handle = new ErlAttribute(source, "hello")
		val body = new SourceElementBody()
		body.fullRange = new TextRange(0, 1)
		addChild(parentBody, handle, body)

		complete(body)

		complete(parentBody)
	}
}
