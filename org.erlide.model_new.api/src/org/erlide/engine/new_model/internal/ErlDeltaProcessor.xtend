package org.erlide.engine.new_model.internal

import java.util.HashSet
import java.util.Set
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceDelta
import org.eclipse.core.resources.IResourceDeltaVisitor
import org.eclipse.core.resources.IWorkspaceRoot
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.IPath
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.IHandleDelta
import org.eclipse.handly.model.impl.Body
import org.eclipse.handly.model.impl.Handle
import org.eclipse.handly.model.impl.HandleDelta
import org.erlide.engine.new_model.ErlModelCore
import org.erlide.engine.new_model.IErlElement
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.new_model.IErlSource
import org.erlide.util.ErlLogger

/**
 * This class is used by the <code>ErlModelManager</code> to process
 * resource deltas and update the Erl Model accordingly.
 */
class ErlDeltaProcessor implements IResourceDeltaVisitor {
    val HandleDelta currentDelta = new HandleDelta(ErlModelCore.getErlModel())
    val Set<String> oldErlProjectNames = new HashSet<String>()

    /**
     * Returns the Erl element delta built from the resource delta.
     * Returns an empty delta if no Erl elements were affected
     * by the resource change.
     *
     * @return Erl element delta (never <code>null</code>)
     */
    def HandleDelta getDelta() {
        currentDelta
    }

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
        initOldErlProjectNames()
        true
    }

    def private boolean processProject(IResourceDelta delta) {
        switch (delta.getKind()) {
            case IResourceDelta.ADDED:
                processAddedProject(delta)
            case IResourceDelta.REMOVED:
                processRemovedProject(delta)
            case IResourceDelta.CHANGED:
                return processChangedProject(delta)
            default:
                true
        }
    }

    def private boolean processAddedProject(IResourceDelta delta) {
        val IProject project = delta.getResource() as IProject
        if (project.hasNature(IErlProject.NATURE_ID)) {
            val IErlProject erlProject = ErlModelCore.create(project)
            addToModel(erlProject)
            translateAddedDelta(delta, erlProject)
        } else {
            addResourceDelta(delta)
        }
        false
    }

    def private boolean processRemovedProject(IResourceDelta delta) {
        val IProject project = delta.getResource() as IProject
        if (wasErlProject(project)) {
            val IErlProject erlProject = ErlModelCore.create(project)
            removeFromModel(erlProject)
            translateRemovedDelta(delta, erlProject)
        } else {
            addResourceDelta(delta)
        }
        false
    }

    def private boolean processChangedProject(IResourceDelta delta) {
        val IProject project = delta.getResource() as IProject
        val IErlProject erlProject = ErlModelCore.create(project)

        if ((delta.getFlags().bitwiseAnd(IResourceDelta.OPEN)) != 0) {
            if (project.isOpen()) {
                if (project.hasNature(IErlProject.NATURE_ID)) {
                    addToModel(erlProject)
                    currentDelta.insertAdded(erlProject, IHandleDelta.F_OPEN)
                } else {
                    addResourceDelta(delta)
                }
            } else {
                if (wasErlProject(project)) {
                    removeFromModel(erlProject)
                    currentDelta.insertRemoved(erlProject, IHandleDelta.F_OPEN)
                } else {
                    addResourceDelta(delta)
                }
            }
            return false
        }

        val boolean isErlProject = project.hasNature(IErlProject.NATURE_ID)
        if ((delta.getFlags().bitwiseAnd(IResourceDelta.DESCRIPTION) ) != 0) {

            val boolean wasErlProject = wasErlProject(project)
            if (wasErlProject != isErlProject) {

                // Erl nature has been added or removed
                if (isErlProject) {
                    addToModel(erlProject)
                    currentDelta.insertAdded(erlProject, IHandleDelta.F_DESCRIPTION)
                } else {
                    removeFromModel(erlProject)
                    currentDelta.insertRemoved(erlProject, IHandleDelta.F_DESCRIPTION)
                }
                return false // when Erl nature is added/removed don't process children
            } else {
                if (isErlProject) {
                    currentDelta.insertChanged(erlProject, IHandleDelta.F_DESCRIPTION)
                }
            }
        }

        if (isErlProject) {
            val Body parentBody = findBody(erlProject.getParent())
            val children = parentBody.getChildren()
            if (!children.contains(erlProject))
                addToModel(erlProject) // in case the project was removed then added then changed

            return true
        } else {
            return false
        }
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

    def private boolean processAddedFile(IResourceDelta delta) {
        val IFile file = delta.resource as IFile
        val IErlSource erlFile = ErlModelCore.create(file)
        if (erlFile !== null) {
            addToModel(erlFile)
            translateAddedDelta(delta, erlFile)
        } else {
            addResourceDelta(delta)
        }
        false
    }

    def private boolean processRemovedFile(IResourceDelta delta) {
        val IFile file = delta.resource as IFile
        val IErlSource erlFile = ErlModelCore.create(file)
        if (erlFile !== null) {
            removeFromModel(erlFile)
            translateRemovedDelta(delta, erlFile)
        } else {
            addResourceDelta(delta)
        }
        false
    }

    def private boolean processChangedFile(IResourceDelta delta) {
        val IFile file = delta.resource as IFile
        val IErlSource erlFile = ErlModelCore.create(file)
        if (erlFile !== null) {
            if ((delta.getFlags().bitwiseAnd(IResourceDelta.MARKERS.bitwiseOr(IResourceDelta.SYNC)).bitwiseNot) != 0) {
                contentChanged(erlFile)
            }
        } else {
            addResourceDelta(delta)
        }
        return false
    }

    def private contentChanged(IErlSource source) {
        close(source)
        currentDelta.insertChanged(source, IHandleDelta.F_CONTENT)
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

    def private void translateAddedDelta(IResourceDelta delta, IErlElement element) {
        if ((delta.getFlags().bitwiseAnd(IResourceDelta.MOVED_FROM)) == 0) {

            // regular addition
            currentDelta.insertAdded(element)
        } else {
            val IErlElement movedFromElement = ErlModelCore.create(
                getResource(delta.getMovedFromPath(), delta.getResource().getType()))
            if (movedFromElement === null)
                currentDelta.insertAdded(element)
            else
                currentDelta.insertMovedTo(element, movedFromElement)
        }
    }

    def private void translateRemovedDelta(IResourceDelta delta, IErlElement element) {
        if ((delta.getFlags().bitwiseAnd(IResourceDelta.MOVED_TO) ) == 0) {

            // regular removal
            currentDelta.insertRemoved(element)
        } else {
            val IErlElement movedToElement = ErlModelCore.create(
                getResource(delta.getMovedToPath(), delta.getResource().getType()))
            if (movedToElement === null)
                currentDelta.insertRemoved(element)
            else
                currentDelta.insertMovedFrom(element, movedToElement)
        }
    }

    def private static IResource getResource(IPath fullPath, int resourceType) {
        val IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot()
        switch (resourceType) {
            case IResource.ROOT:
                return root
            case IResource.PROJECT:
                return root.getProject(fullPath.lastSegment())
            case IResource.FOLDER:
                return root.getFolder(fullPath)
            case IResource.FILE:
                return root.getFile(fullPath)
            default:
                return null
        }
    }

    def private void initOldErlProjectNames() {
        val erlProjects = ErlModelCore.getErlModel().getProjects()
        for (IErlProject erlProject : erlProjects) {
            oldErlProjectNames.add(erlProject.getName())
        }
    }

    def private boolean wasErlProject(IProject project) {
        oldErlProjectNames.contains(project.getName())
    }

    def private void addResourceDelta(IResourceDelta delta) {
        var HandleDelta handleDelta
        val IResource parent = delta.resource.parent
        if (parent instanceof IWorkspaceRoot)
            handleDelta = currentDelta
        else if (parent instanceof IProject) {
            val IErlProject fooProject = ErlModelCore.create(parent)
            handleDelta = currentDelta.getDeltaFor(fooProject)
            if (handleDelta === null) {
                handleDelta = new HandleDelta(fooProject)
                currentDelta.insert(handleDelta)
            }
            if ((delta.getKind().bitwiseAnd(IResourceDelta.ADDED.bitwiseOr(IResourceDelta.REMOVED))) != 0) {

                // reset non-Erl resources
                val ErlProjectBody body = findBody(fooProject) as ErlProjectBody
                if (body !== null)
                    body.setNonErlResources(null)
            }
        } else if (parent instanceof IFolder) {
            // TODO
            return
        } else {
            ErlLogger.warn('''addResourceDelta ? parent=«parent»''')
            throw new AssertionError()
        }
        handleDelta.addResourceDelta(delta)
    }
}
