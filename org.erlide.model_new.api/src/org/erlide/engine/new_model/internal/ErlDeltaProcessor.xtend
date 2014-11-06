package org.erlide.engine.new_model.internal

import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceDelta
import org.eclipse.core.resources.IResourceDeltaVisitor
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.Handle
import org.erlide.engine.new_model.ErlModelCore
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.new_model.IErlSource

/**
 * This class is used by the <code>FooModelManager</code> to process
 * resource deltas and update the Foo Model accordingly.
 */
class ErlDeltaProcessor implements IResourceDeltaVisitor {
	override boolean visit(IResourceDelta delta) {
		switch (delta.getResource().getType()) {
			case IResource.ROOT:
				processRoot(delta)
			case IResource.PROJECT:
				processProject(delta)
			case IResource.FILE:
				processFile(delta)
			default:
				true
		}
	}

	def private boolean processRoot(IResourceDelta delta) {
		true
	}

	def private boolean processProject(IResourceDelta delta) {
		switch (delta.getKind()) {
			case IResourceDelta.ADDED:
				processAddedProject(delta)
			case IResourceDelta.REMOVED:
				processRemovedProject(delta)
			default:
				true
		}
	}

	def private boolean processAddedProject(IResourceDelta delta) {
		val IProject project = delta.getResource() as IProject
		if (project.open && project.hasNature(IErlProject.NATURE_ID)) {
			val IErlProject erlProject = ErlModelCore.create(project)
			addToModel(erlProject)
		}
		false
	}

	def private boolean processRemovedProject(IResourceDelta delta) {
		val IProject project = delta.getResource() as IProject
		val IErlProject erlProject = ErlModelCore.create(project)
		removeFromModel(erlProject)
		false
	}

	def private boolean processFile(IResourceDelta delta) {
		switch (delta.getKind()) {
			case IResourceDelta.ADDED:
				processAddedFile(delta)
			case IResourceDelta.REMOVED:
				processRemovedFile(delta)
			case IResourceDelta.CHANGED:
				processChangedFile(delta)
			default:
				true
		}
	}

	def boolean processAddedFile(IResourceDelta delta) {
		val IFile file = delta.resource as IFile
		val IErlSource erlFile = ErlModelCore.create(file)
		if (erlFile != null)
			addToModel(erlFile)
		false
	}

	def boolean processRemovedFile(IResourceDelta delta) {
		val IFile file = delta.resource as IFile
		val IErlSource erlFile = ErlModelCore.create(file)
		if (erlFile != null)
			removeFromModel(erlFile)
		false
	}

	def boolean processChangedFile(IResourceDelta delta) {
		val IFile file = delta.resource as IFile
		val IErlSource erlFile = ErlModelCore.create(file)
		if (erlFile != null) {
			if ((delta.getFlags().bitwiseAnd(IResourceDelta.MARKERS.bitwiseOr(IResourceDelta.SYNC)).bitwiseNot) != 0) {
				contentChanged(erlFile)
			}
		}
		return false
	}

	def contentChanged(IErlSource source) {
		close(source)
	}

	def private static void addToModel(IHandle element) {
		val Body parentBody = findBody(element.getParent())
		if (parentBody !== null)
			parentBody.addChild(element)
		close(element)
	}

	def private static void removeFromModel(IHandle element) {
		val Body parentBody = findBody(element.getParent())
		if (parentBody !== null)
			parentBody.removeChild(element)
		close(element)
	}

	def private static Body findBody(IHandle element) {
		(element as Handle).findBody()
	}

	def private static void close(IHandle element) {
		(element as Handle).close()
	}
}
