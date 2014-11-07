package org.erlide.engine.new_model

import java.net.URI
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IWorkspace
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.handly.model.IElementChangeListener
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.ISourceFile
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.model.root.ErlangProjectProperties

interface IErlElement extends IHandle {
}

interface IErlModel extends IErlElement {
	def IWorkspace getWorkspace()

	def Iterable<IErlProject> getProjects()

	def IErlProject getProject(String name)

	/**
     * Adds the given listener for changes to elements in the Erlang Model.
     * Has no effect if an identical listener is already registered.
     * <p>
     * Once registered, a listener starts receiving notification
     * of changes to elements in the Erlang Model. The listener continues
     * to receive notifications until it is removed.
     * </p>
     *
     * @param listener the listener (not <code>null</code>)
     * @see #removeElementChangeListener(IElementChangeListener)
     */
	def void addElementChangeListener(IElementChangeListener listener)

	/**
     * Removes the given element change listener.
     * Has no effect if an identical listener is not registered.
     *
     * @param listener the listener (not <code>null</code>)
     */
	def void removeElementChangeListener(IElementChangeListener listener)
}

interface IErlLibrary extends IErlElement, IErlLibraryContainer {
	def ErlangLibraryProperties getProperties()

	def Iterable<IErlFolder> getFolders()

	def Iterable<IErlFolder> getSourceFolders()

	def Iterable<IErlFolder> getIncludeFolders()

	def IErlFolder getBinaryFolder()

	def Iterable<IResource> getNonErlangResources()
}

interface IErlProject extends IErlLibrary, IErlLibraryContainer {
	val static String NATURE_ID = "org.erlide.core.erlnature"

	/**
     * Creates a new Erlang project in the workspace with files in the default
     * location in the local file system.
     * <p>
     * This is a convenience method, fully equivalent to:
     * <pre>  create(null, monitor);  </pre>
     * </p><p>
     * This method changes resources; these changes will be reported
     * in a subsequent change event, including an indication that
     * this project has been added to the workspace.
     * </p><p>
     * This method is long-running; progress and cancellation are provided
     * by the given progress monitor.
     * </p>
     *
     * @param monitor a progress monitor, or <code>null</code> if progress
     *  reporting is not desired
     * @throws CoreException if the project could not be created.
     *  Reasons include:
     * <ul>
     * <li> The project already exists in the workspace.</li>
     * <li> The name of the project is not valid (according to
     *    <code>IWorkspace.validateName</code>).</li>
     * <li> The project description file could not be created in the project
     *      content area.</li>
     * <li> Resource changes are disallowed during certain types of resource change
     *       event notification. See <code>IResourceChangeEvent</code> for more details.</li>
     * </ul>
     * @see #create(URI, IProgressMonitor)
     */
	def void create(IProgressMonitor monitor) throws CoreException

	/**
     * Creates a new Erlang project in the workspace. Upon successful completion,
     * the corresponding project resource will exist and be open.
     * <p>
     * The created project resource will have the following configuration:
     * <ul>
     * <li>the given project location</li>
     * <li>no references to other projects</li>
     * <li>Xtext project nature and builder</li>
     * <li>Erlang project nature</li>
     * <li>UTF-8 as the default charset</li>
     * </ul>
     * </p><p>
     * This method changes resources; these changes will be reported
     * in a subsequent change event, including an indication that
     * this project has been added to the workspace.
     * </p><p>
     * This method is long-running; progress and cancellation are provided
     * by the given progress monitor.
     * </p>
     *
     * @param location the location for the project.
     *  If <code>null</code> is specified, the default location is used.
     * @param monitor a progress monitor, or <code>null</code> if progress
     *  reporting is not desired
     * @throws CoreException if the project could not be created.
     *  Reasons include:
     * <ul>
     * <li> The project already exists in the workspace.</li>
     * <li> The name of the project is not valid (according to
     *    <code>IWorkspace.validateName</code>).</li>
     * <li> The project description file could not be created in the project
     *      content area.</li>
     * <li> Resource changes are disallowed during certain types of resource change
     *       event notification. See <code>IResourceChangeEvent</code> for more details.</li>
     * </ul>
     * @see IWorkspace#validateProjectLocationURI(IProject, URI)
     */
	def void create(URI location, IProgressMonitor monitor) throws CoreException

	def IProject getWorkspaceProject()

	def ErlangProjectProperties getProjectProperties()

	/**
     * Returns the non-Erlang resources contained in this project.
     *
     * @return the non-Erlang resources contained in this project (never <code>null</code>)
     * @throws CoreException if this element does not exist or if an exception
     *  occurs while accessing its corresponding resource
     */
	def IResource[] getNonErlResources()

	def Iterable<IErlSource> getSourceFiles()

	def IErlSource getSourceFile(String name)

	def Iterable<IErlModule> getModules()

	def Iterable<IErlHeader> getHeaders()

	def IErlOtpLibrary getOtpLibrary()
}

interface IErlLibraryContainer extends IErlElement {
	def Iterable<IErlLibrary> getLibraries()
}

interface IErlOtpLibrary extends IErlLibrary {
	def String getVersion()
}

interface IErlFolder extends IErlElement {
	def IFolder getWorkspaceFolder()

	def Iterable<IErlSource> getSources()
}

interface IErlSource extends IErlElement, ISourceFile {
	def Iterable<IErlForm> getForms()

	def String getExtension()

	def IErlComment getHeaderComment()

	def Iterable<IErlAttribute> getAttributes()

	def Iterable<IErlAttribute> getAttributesWithTag(String tag)

	def Iterable<IErlFunction> getFunctions()

	def IErlFunction getFunction(String name, int arity)

	def Iterable<IErlError> getErrors()
}

public interface IErlModule extends IErlSource {
}

public interface IErlHeader extends IErlSource {
}
