package org.erlide.engine.new_model.internal

import java.util.List
import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.impl.Body
import org.erlide.engine.new_model.ErlModelCore
import org.erlide.engine.new_model.IErlProject

class ErlProjectBody extends Body {
    var IResource[] nonErlResources

    def Iterable<IResource> getNonErlResources(IErlProject erlProject) throws CoreException
    {
        if (nonErlResources === null) {
            val resource = erlProject.getWorkspaceProject()
            nonErlResources = computeNonErlResources(resource)
        }
        nonErlResources
    }

    def void setNonErlResources(IResource[] resources) {
        this.nonErlResources = resources
    }

    def private List<IResource> computeNonErlResources(IContainer resource) throws CoreException
    {
        val List<IResource> result = newArrayList()
        val members = resource.members()
        for (member : members) {
            if (member instanceof IFile) {
                if (ErlModelCore.create(member) === null) {
                    result.add(member)
                }
            } else if(member instanceof IContainer) {
                result.addAll(computeNonErlResources(member))
            }
        }
        result
    }

}
