package org.erlide.engine.new_model;

import org.eclipse.core.resources.IWorkspace;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlProject;

@SuppressWarnings("all")
public interface IErlModel extends IErlElement {
  public abstract IWorkspace getWorkspace();
  
  public abstract Iterable<IErlProject> getProjects();
  
  public abstract IErlProject getProject(final String name);
}
