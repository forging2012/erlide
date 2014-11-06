package org.erlide.engine.new_model.internal

import java.util.Map
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.SourceElementBody
import org.eclipse.handly.model.impl.SourceFile
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.new_model.IErlComment
import org.erlide.engine.new_model.IErlForm
import org.erlide.engine.new_model.IErlHeader
import org.erlide.engine.new_model.IErlModule

@Data
abstract class ErlSource extends SourceFile implements IErlModule {

	new(ErlProject parent, IFile file) {
		super(parent, file)
	}

	override protected buildStructure(SourceElementBody body, Map<IHandle, Body> newElements, Object ast, String source) {
		// TODO nothing for now
	}

	override protected createStructuralAst(String source) throws CoreException {

		// TODO nothing for now
		null
	}

	override protected getHandleManager() {
		ErlModelManager.INSTANCE.getHandleManager()
	}

	override getForms() {
		children.map[it as IErlForm]
	}

	override getHeaderComment() {
		val first = children.head
		if (first instanceof IErlComment)
			first
		else
			null
	}
}

@Data
class ErlModule extends ErlSource implements IErlModule {

	new(ErlProject parent, IFile file) {
		super(parent, file)
		if (!file.getParent().equals(parent.resource))
			throw new IllegalArgumentException()
		if (!extension.equals(file.getFileExtension()))
			throw new IllegalArgumentException()
	}

	override getExtension() {
		"erl"
	}

}

@Data
class ErlHeader extends ErlSource implements IErlHeader {

	new(ErlProject parent, IFile file) {
		super(parent, file)
		if (!file.getParent().equals(parent.resource))
			throw new IllegalArgumentException()
		if (!extension.equals(file.getFileExtension()))
			throw new IllegalArgumentException()
	}

	override getExtension() {
		"hrl"
	}

}
