package org.erlide.engine.new_model.internal;

import java.util.List;
import java.util.Map;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.internal.ErlElement;
import org.erlide.engine.new_model.internal.ErlProject;

@Data
@SuppressWarnings("all")
public class ErlModel extends ErlElement implements IErlModel {
  private final IWorkspace workspace;
  
  public ErlModel() {
    super(null, null);
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    this.workspace = _workspace;
  }
  
  public IResource getResource() {
    return this.workspace.getRoot();
  }
  
  public void validateExistence() {
  }
  
  protected void buildStructure(final Body body, final Map<IHandle, Body> newElements) throws CoreException {
    IWorkspaceRoot _root = this.workspace.getRoot();
    final IProject[] projects = _root.getProjects();
    final List<IErlProject> erlProjects = CollectionLiterals.<IErlProject>newArrayList();
    for (final IProject project : projects) {
      boolean _and = false;
      boolean _isOpen = project.isOpen();
      if (!_isOpen) {
        _and = false;
      } else {
        boolean _hasNature = project.hasNature(IErlProject.NATURE_ID);
        _and = _hasNature;
      }
      if (_and) {
        ErlProject _erlProject = new ErlProject(this, project, null);
        erlProjects.add(_erlProject);
      }
    }
    body.setChildren(((IHandle[])Conversions.unwrapArray(erlProjects, IHandle.class)));
  }
  
  public Iterable<IErlProject> getProjects() {
    try {
      return (Iterable<IErlProject>)Conversions.doWrapArray(this.<IErlProject>getChildren(IErlProject.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlProject getProject(final String name) {
    IWorkspaceRoot _root = this.workspace.getRoot();
    IProject _project = _root.getProject(name);
    return new ErlProject(this, _project, null);
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.workspace== null) ? 0 : this.workspace.hashCode());
    return result;
  }
  
  @Override
  @Pure
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    if (!super.equals(obj))
      return false;
    ErlModel other = (ErlModel) obj;
    if (this.workspace == null) {
      if (other.workspace != null)
        return false;
    } else if (!this.workspace.equals(other.workspace))
      return false;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    String result = new ToStringBuilder(this)
    	.addAllFields()
    	.toString();
    return result;
  }
  
  @Pure
  public IWorkspace getWorkspace() {
    return this.workspace;
  }
}
