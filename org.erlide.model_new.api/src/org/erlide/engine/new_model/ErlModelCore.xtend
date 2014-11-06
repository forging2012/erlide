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

	/**
     * Returns the Erlang element corresponding to the given resource, or
     * <code>null</code> if unable to associate the given resource
     * with an element of the Erlang Model.
     *
     * @param resource the given resource (maybe <code>null</code>)
     * @return the Erlang element corresponding to the given resource, or
     *  <code>null</code> if unable to associate the given resource
     *  with an element of the Erlang Model
     */
	def static IErlElement create(IResource resource) {
		if (resource == null)
			return null;
		val type = resource.getType();
		switch type {
			case IResource.PROJECT:
				create(resource as IProject)
			case IResource.FILE:
				create(resource as IFile)
			case IResource.ROOT:
				getErlModel()
			default:
				null
		}
	}

	private new() {
	}
}
