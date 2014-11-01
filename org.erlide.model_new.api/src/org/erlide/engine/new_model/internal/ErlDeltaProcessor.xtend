package org.erlide.engine.new_model.internal

import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceDelta
import org.eclipse.core.resources.IResourceDeltaVisitor
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.Handle
import org.erlide.engine.new_model.ErlModelCore
import org.erlide.engine.new_model.IErlProject

/**
 * This class is used by the <code>FooModelManager</code> to process
 * resource deltas and update the Foo Model accordingly.
 */
class ErlDeltaProcessor implements IResourceDeltaVisitor {
    override boolean visit(IResourceDelta delta) throws CoreException
    {
        switch (delta.getResource().getType()) {
            case IResource.ROOT:
                return processRoot(delta)
            case IResource.PROJECT:
                return processProject(delta)
            default:
                return true
        }
    }

    def private boolean processRoot(IResourceDelta delta) throws CoreException
    {
        return true;
    }

    def private boolean processProject(IResourceDelta delta) throws CoreException
{
        switch (delta.getKind()) {
            case IResourceDelta.ADDED:
                return processAddedProject(delta)
            case IResourceDelta.REMOVED:
                return processRemovedProject(delta)
            default:
                return true
        }
    }

    def private boolean processAddedProject(IResourceDelta delta) throws CoreException
{
        val IProject project = delta.getResource() as IProject;
        if (project.hasNature(IErlProject.NATURE_ID)) {
            val IErlProject erlProject = ErlModelCore.create(project)
            addToModel(erlProject)
        }
        return false
    }

    def private boolean processRemovedProject(IResourceDelta delta) throws CoreException
{
        val IProject project = delta.getResource() as IProject
        val IErlProject erlProject = ErlModelCore.create(project)
        removeFromModel(erlProject)
        return false
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
        return (element as Handle).findBody()
    }

    def private static void close(IHandle element) {
        (element as Handle).close()
    }
}
