package org.erlide.engine.new_model;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
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
