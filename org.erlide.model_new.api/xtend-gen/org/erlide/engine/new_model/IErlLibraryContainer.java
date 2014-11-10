package org.erlide.engine.new_model;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlLibrary;

@SuppressWarnings("all")
public interface IErlLibraryContainer extends IErlElement {
  public abstract IErlLibrary getLibrary(final String name);
  
  public abstract Iterable<IErlLibrary> getLibraries() throws CoreException;
}
