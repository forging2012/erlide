package org.erlide.engine.new_model.internal

import java.util.List
import java.util.Map
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IWorkspace
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.erlide.engine.new_model.IErlModel
import org.erlide.engine.new_model.IErlProject
import org.eclipse.xtend.lib.annotations.Data

@Data
class ErlModel extends ErlElement implements IErlModel {
    val IWorkspace workspace

    new() {
        super(null, null)
        workspace = ResourcesPlugin.workspace
    }

    override getResource() {
        workspace.root
    }

    override validateExistence() {
        // always exists
    }

    override protected buildStructure(Body body, Map<IHandle, Body> newElements) throws CoreException {
        val IProject[] projects = workspace.getRoot().getProjects()
        val List<IErlProject> erlProjects = newArrayList()
        for (IProject project : projects) {
            if (project.isOpen() && project.hasNature(IErlProject.NATURE_ID)) {
                erlProjects.add(new ErlProject(this, project, null)) // TODO fix
            }
        }
        body.setChildren(erlProjects)
    }

    override getProjects() {
        getChildren(IErlProject)
    }

    override getProject(String name) {
        new ErlProject(this, workspace.root.getProject(name), null) // TODO fix
    }

}
