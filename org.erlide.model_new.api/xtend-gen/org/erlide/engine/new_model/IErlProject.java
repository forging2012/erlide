package org.erlide.engine.new_model;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.new_model.IErlLibrary;
import org.erlide.engine.new_model.IErlLibraryContainer;
import org.erlide.engine.new_model.IErlModule;
import org.erlide.engine.new_model.IErlOtpLibrary;

@SuppressWarnings("all")
public interface IErlProject extends IErlLibrary, IErlLibraryContainer {
  public final static String NATURE_ID = "org.erlide.core.erlnature";
  
  public abstract IProject getWorkspaceProject();
  
  public abstract ErlangProjectProperties getProjectProperties();
  
  public abstract Iterable<IErlModule> getModules();
  
  public abstract IErlOtpLibrary getOtpLibrary();
}
