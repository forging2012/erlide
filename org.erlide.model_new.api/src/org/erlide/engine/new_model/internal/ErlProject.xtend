package org.erlide.engine.new_model.internal

import java.net.URI
import java.text.MessageFormat
import java.util.List
import java.util.Map
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IProjectDescription
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IWorkspaceRunnable
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.SubProgressMonitor
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.NewModelActivator
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.new_model.IErlFolder
import org.erlide.engine.new_model.IErlModule
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.new_model.IErlSource

@Data
class ErlProject extends ErlLibrary implements IErlProject {

    transient IProject workspaceProject

    new(ErlModel parent, IProject workspaceProject, ErlangLibraryProperties properties) {
        super(parent, workspaceProject.name, properties)
        this.workspaceProject = workspaceProject
    }

    override getWorkspaceProject() {
        workspaceProject
    }

    override void create(IProgressMonitor monitor) throws CoreException
    {
        create(null, monitor)
    }

    override void create(URI location, IProgressMonitor monitor) {
        val workspace = (getParent() as ErlModel).getWorkspace()
        workspace.run(
            new IWorkspaceRunnable() {
                override void run(IProgressMonitor monitor0) {
                    val monitor = monitor0 ?: new NullProgressMonitor()
                    try {
                        monitor.beginTask("", 4)

                        val IProjectDescription description = workspace.newProjectDescription(name)
                        description.setLocationURI(location)
                        workspaceProject.create(description, new SubProgressMonitor(monitor, 1))
                        workspaceProject.open(new SubProgressMonitor(monitor, 1))

                        description.setNatureIds(#[IErlProject.NATURE_ID])
                        workspaceProject.setDescription(description, new SubProgressMonitor(monitor, 1))

                        workspaceProject.setDefaultCharset("UTF-8", new SubProgressMonitor(monitor, 1))
                    } finally {
                        monitor.done()
                    }
                }
            }, monitor)
    }

    override IResource getResource() {
        workspaceProject
    }

    override getProjectProperties() {
        properties as ErlangProjectProperties
    }

    override buildStructure(Body body, Map<IHandle, Body> newElements) throws CoreException {

        // TODO new_model
        val IResource[] members = workspaceProject.members()
        val List<IErlSource> erlFiles = newArrayList()
        for (IResource file : members) {
            if (file instanceof IFile) {
                val IErlSource source = createSourceFile(file)
                if (source !== null)
                    erlFiles.add(source)
            }
        }
        body.setChildren(erlFiles)
    }

    // TODO new_model: delegate to ErlSource? factory?
    private def IErlSource createSourceFile(IFile file) {
        switch file.fileExtension {
            case "erl":
                new ErlModule(this, file)
            case "hrl":
                new ErlHeader(this, file)
            default:
                null
        }
    }

    override getNonErlResources() {
        return (getBody() as ErlProjectBody).getNonErlResources(this)
    }

    override getSourceFile(String name) {
        createSourceFile(workspaceProject.getFile(name))
    }

    override getModules() {
        getDeepChildren(sourceFolders, IErlModule)
    }

    override getOtpLibrary() {

        //TODO: auto-generated method stub
        null
    }

    override getLibraries() {

        //TODO: auto-generated method stub
        null
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

    override newBody() {
        new ErlProjectBody()
    }

    def <T extends IHandle> Iterable<T> getDeepChildren(Iterable<IErlFolder> folders, Class<T> clazz) {
        val List<T> result = newArrayList()
        for (folder : folders) {
            result.addAll(folder.getChildren(clazz))
        }
        result
    }

}
