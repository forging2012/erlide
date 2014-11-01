package org.erlide.engine.new_model;

import org.eclipse.core.resources.IResource;
import org.erlide.engine.model.root.ErlangLibraryProperties;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlFolder;

@SuppressWarnings("all")
public interface IErlLibrary extends IErlElement {
  public abstract ErlangLibraryProperties getProperties();
  
  public abstract Iterable<IErlFolder> getFolders();
  
  public abstract Iterable<IErlFolder> getSourceFolders();
  
  public abstract Iterable<IErlFolder> getIncludeFolders();
  
  public abstract IErlFolder getOutputFolder();
  
  public abstract Iterable<IErlLibrary> getReferencedLibraries();
  
  public abstract Iterable<IResource> getNonErlangResources();
}
