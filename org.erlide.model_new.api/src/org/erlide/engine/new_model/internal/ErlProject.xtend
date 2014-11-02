package org.erlide.engine.new_model.internal

import java.text.MessageFormat
import java.util.List
import java.util.Map
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.new_model.IErlSource
import org.erlide.engine.NewModelActivator

@Data
class ErlProject extends ErlLibrary implements IErlProject {

    IProject workspaceProject

    new(ErlModel parent, IProject workspaceProject, ErlangLibraryProperties properties) {
        super(parent, workspaceProject.name, properties)
        this.workspaceProject = workspaceProject
    }

    override getResource() {
        workspaceProject
    }

    override getProjectProperties() {
        properties as ErlangProjectProperties
    }

    protected override buildStructure(Body body, Map<IHandle, Body> newElements) throws CoreException {

        // TODO fix
        val IResource[] members = workspaceProject.members()
        val List<IErlSource> erlFiles = newArrayList();
        for (IResource file : members) {
            if (file instanceof IFile) {
                val ext = file.fileExtension
                val IErlSource source = createInstance(ext, file)
                if (source !== null)
                    erlFiles.add(source);
            }
        }
        body.setChildren(erlFiles);
    }

    // TODO delegate to ErlSource? factory?
    private def createInstance(String ext, IFile file) {
        switch ext {
            case "erl":
                new ErlModule(this, file)
            case "hrl":
                new ErlHeader(this, file)
            default:
                null
        }
    }

    override getModules() {
        throw new UnsupportedOperationException("TODO: auto-generated method stub")
    }

    override getOtpLibrary() {
        throw new UnsupportedOperationException("TODO: auto-generated method stub")
    }

    override getLibraries() {
        throw new UnsupportedOperationException("TODO: auto-generated method stub")
    }

    override protected validateExistence() throws CoreException {
        if (!workspaceProject.exists())
            throw new CoreException(
                NewModelActivator.createErrorStatus(
                    MessageFormat.format("Project ''{0}'' does not exist in workspace", name), null))

        if (!workspaceProject.isOpen())
            throw new CoreException(
                NewModelActivator.createErrorStatus(MessageFormat.format("Project ''{0}'' is not open", name), null))

        if (!workspaceProject.hasNature(NATURE_ID))
            throw new CoreException(
                NewModelActivator.createErrorStatus(
                    MessageFormat.format("Project ''{0}'' does not have the erlang nature", name), null))
    }

}
