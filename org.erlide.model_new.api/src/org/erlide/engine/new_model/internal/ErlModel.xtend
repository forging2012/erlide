package org.erlide.engine.new_model.internal

import java.util.List
import java.util.Map
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IWorkspace
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IElementChangeListener
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.new_model.IErlModel
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.model.root.ErlangProjectProperties

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
                erlProjects.add(new ErlProject(this, project, getProjectProperties(project)))
            }
        }
        body.setChildren(erlProjects)
    }

    def getProjectProperties(IProject project) {

        // TODO new_model
        new ErlangProjectProperties()
    }

    override getProjects() {
        getChildren(IErlProject)
    }

    override getProject(String name) {

        // TODO new_model
        new ErlProject(this, workspace.root.getProject(name), new ErlangProjectProperties())
    }

    override void addElementChangeListener(IElementChangeListener listener) {
        ErlModelManager.INSTANCE.addElementChangeListener(listener)
    }

    override void removeElementChangeListener(IElementChangeListener listener) {
        ErlModelManager.INSTANCE.removeElementChangeListener(listener)
    }
}
