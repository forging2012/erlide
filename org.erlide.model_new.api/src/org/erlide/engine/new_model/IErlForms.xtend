package org.erlide.engine.new_model

import org.eclipse.handly.model.ISourceConstruct
import java.util.Map

interface IErlForm extends IErlElement, ISourceConstruct {
	def IErlModule getModule()

	// API to interact with the erlang-based model?
}

interface IErlAttribute extends IErlForm {
	def Iterable<IErlExpression> getValues()
}

interface IErlFunction extends IErlForm {
	def int getArity()

	def Iterable<IErlFunctionClause> getClauses()

	def IErlTypeSpec getTypeSpecification()

	def IErlComment getComment()
}

interface IErlComment extends IErlForm {
	def String getValue()

	def int getLevel()
}

interface IErlError extends IErlForm {
	def String getMessage()
}

interface IErlModuleDef extends IErlAttribute {
}

interface IErlRecordDef extends IErlAttribute {
	def IErlExpression getDefinition()
}

interface IErlMacroDef extends IErlAttribute {
	def IErlExpression getValue()
}

interface IErlTypeDef extends IErlAttribute {
	def IErlExpression getDefinition()
}

interface IErlExport extends IErlAttribute {
	def Iterable<IErlFunctionRef> getFunctions()
}

interface IErlImport extends IErlAttribute {
	def Map<String, IErlFunctionRef> getFunctions()
}

interface IErlCompilerOpts extends IErlAttribute {
	def IErlExpression getValue()
}

interface IErlIf extends IErlAttribute {
	def Iterable<IErlForm> getIfForms()

	def Iterable<IErlForm> getElseForms()
}

interface IErlBehaviour extends IErlAttribute {
}

interface IErlCallback extends IErlAttribute {
}

interface IErlTypeSpec extends IErlAttribute {
	def IErlFunction getFunction()

	def IErlExpression getSpec()
}

interface IErlCustomAttribute extends IErlAttribute {
	def String getTag()
}

interface IErlFunctionClause extends IErlForm {
	def Iterable<IErlExpression> getFormalParameters()

	def IErlGuard getGuard()

	def IErlExpression getFunctionBody()
}

interface IErlExpression extends IErlElement, ISourceConstruct {
	def String getContent()
}

interface IErlGuard extends IErlExpression {
}

interface IErlFunctionRef {
	def String getModule()

	def String getName()

	def int getArity()
}
