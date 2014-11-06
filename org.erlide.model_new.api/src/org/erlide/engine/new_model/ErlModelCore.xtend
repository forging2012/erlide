package org.erlide.engine.new_model

import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.erlide.engine.new_model.internal.ErlModelManager

class ErlModelCore {

	def static IErlModel getErlModel() {
		ErlModelManager.INSTANCE.erlModel
	}

	def static IErlProject create(IProject project) {
		if (project === null)
			null
		else
			erlModel.getProject(project.name)
	}

	def static IErlSource create(IFile file) {
		if (file === null)
			return null

		// TODO fixme
		if (file.parent.type != IResource.PROJECT)
			return null
		val project = create(file.project)
		project.getSourceFile(file.name)
	}

	private new() {
	}
}
