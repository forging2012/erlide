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
import org.erlide.engine.new_model.IErlHeader
import org.erlide.engine.new_model.IErlModule
import org.erlide.engine.new_model.IErlProject
import org.erlide.engine.new_model.IErlSource

@Data
class ErlProject extends ErlLibrary implements IErlProject {

	IProject workspaceProject

	new(ErlModel parent, IProject workspaceProject, ErlangLibraryProperties properties) {
		super(parent, workspaceProject.name, properties)
		this.workspaceProject = workspaceProject
	}

	override void create(IProgressMonitor monitor) throws CoreException
    {
		create(null, monitor)
	}

	override void create(URI location, IProgressMonitor monitor) {
		val workspace = getParent().getWorkspace()
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

	override ErlModel getParent() {
		parent as ErlModel
	}

	override getResource() {
		workspaceProject
	}

	override getProjectProperties() {
		properties as ErlangProjectProperties
	}

	override buildStructure(Body body, Map<IHandle, Body> newElements) throws CoreException {

		// TODO fix
		val IResource[] members = workspaceProject.members()
		val List<IErlSource> erlFiles = newArrayList()
		for (IResource file : members) {
			if (file instanceof IFile) {
				val IErlSource source = createInstance(file)
				if (source !== null)
					erlFiles.add(source)
			}
		}
		body.setChildren(erlFiles)
	}

	// TODO delegate to ErlSource? factory?
	private def createInstance(IFile file) {
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

	override getSourceFiles() {
		getChildren(IErlSource)
	}

	override getSourceFile(String name) {
		createInstance(workspaceProject.getFile(name))
	}

	override getModules() {
		getChildren(IErlModule)
	}

	override getHeaders() {
		getChildren(IErlHeader)
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

	override newBody() {
		new ErlProjectBody()
	}
}
