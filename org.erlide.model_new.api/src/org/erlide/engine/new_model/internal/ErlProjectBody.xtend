package org.erlide.engine.new_model.internal

import java.util.ArrayList
import java.util.List
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.impl.Body
import org.erlide.engine.new_model.ErlModelCore
import org.erlide.engine.new_model.IErlProject

class ErlProjectBody extends Body {
    var IResource[] nonErlResources

    def IResource[] getNonErlResources(IErlProject fooProject) throws CoreException
    {
        if (nonErlResources === null)
            nonErlResources = computeNonErlResources(fooProject)
        nonErlResources
    }

    def void setNonErlResources(IResource[] resources) {
        this.nonErlResources = resources
    }

    def private IResource[] computeNonErlResources(IErlProject fooProject) throws CoreException
    {
        val List<IResource> result = new ArrayList<IResource>()
        val IResource[] members = fooProject.getWorkspaceProject().members()
        for (member : members) {
            if (!(member instanceof IFile))
                result.add(member)
            else {
                if (ErlModelCore.create(member as IFile) === null)
                    result.add(member)
            }
        }
        result
    }

}
