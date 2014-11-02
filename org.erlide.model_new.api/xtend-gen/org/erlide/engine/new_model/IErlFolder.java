package org.erlide.engine.new_model;

import org.eclipse.core.resources.IFolder;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlSource;

@SuppressWarnings("all")
public interface IErlFolder extends IErlElement {
  public abstract IFolder getWorkspaceFolder();
  
  public abstract Iterable<IErlSource> getSources();
}
