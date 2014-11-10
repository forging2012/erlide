package org.erlide.engine.new_model;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlFolder;
import org.erlide.engine.new_model.IErlLibraryContainer;

@SuppressWarnings("all")
public interface IErlLibrary extends IErlElement, IErlLibraryContainer {
  public abstract ErlangLibraryProperties getProperties();
  
  public abstract Iterable<IErlFolder> getFolders() throws CoreException;
  
  public abstract Iterable<IErlFolder> getSourceFolders() throws CoreException;
  
  public abstract Iterable<IErlFolder> getIncludeFolders() throws CoreException;
  
  public abstract IErlFolder getBinaryFolder();
  
  public abstract Iterable<IResource> getNonErlangResources() throws CoreException;
}
