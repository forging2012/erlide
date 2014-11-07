package org.erlide.engine.new_model;

import java.net.URI;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.new_model.IErlHeader;
import org.erlide.engine.new_model.IErlLibrary;
import org.erlide.engine.new_model.IErlLibraryContainer;
import org.erlide.engine.new_model.IErlModule;
import org.erlide.engine.new_model.IErlOtpLibrary;
import org.erlide.engine.new_model.IErlSource;

@SuppressWarnings("all")
public interface IErlProject extends IErlLibrary, IErlLibraryContainer {
  public final static String NATURE_ID = "org.erlide.core.erlnature";
  
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
  public abstract void create(final IProgressMonitor monitor) throws CoreException;
  
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
  public abstract void create(final URI location, final IProgressMonitor monitor) throws CoreException;
  
  public abstract IProject getWorkspaceProject();
  
  public abstract ErlangProjectProperties getProjectProperties();
  
  /**
   * Returns the non-Erlang resources contained in this project.
   * 
   * @return the non-Erlang resources contained in this project (never <code>null</code>)
   * @throws CoreException if this element does not exist or if an exception
   *  occurs while accessing its corresponding resource
   */
  public abstract IResource[] getNonErlResources();
  
  public abstract Iterable<IErlSource> getSourceFiles();
  
  public abstract IErlSource getSourceFile(final String name);
  
  public abstract Iterable<IErlModule> getModules();
  
  public abstract Iterable<IErlHeader> getHeaders();
  
  public abstract IErlOtpLibrary getOtpLibrary();
}
