package org.erlide.engine.new_model.internal;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;

@SuppressWarnings("all")
public class ErlProjectBody extends Body {
  private IResource[] nonErlResources;
  
  public IResource[] getNonErlResources(final IErlProject fooProject) throws CoreException {
    IResource[] _xblockexpression = null;
    {
      boolean _tripleEquals = (this.nonErlResources == null);
      if (_tripleEquals) {
        IResource[] _computeNonErlResources = this.computeNonErlResources(fooProject);
        this.nonErlResources = _computeNonErlResources;
      }
      _xblockexpression = this.nonErlResources;
    }
    return _xblockexpression;
  }
  
  public void setNonErlResources(final IResource[] resources) {
    this.nonErlResources = resources;
  }
  
  private IResource[] computeNonErlResources(final IErlProject fooProject) throws CoreException {
    List<IResource> _xblockexpression = null;
    {
      final List<IResource> result = new ArrayList<IResource>();
      IProject _workspaceProject = fooProject.getWorkspaceProject();
      final IResource[] members = _workspaceProject.members();
      for (final IResource member : members) {
        if ((!(member instanceof IFile))) {
          result.add(member);
        } else {
          IErlSource _create = ErlModelCore.create(((IFile) member));
          boolean _tripleEquals = (_create == null);
          if (_tripleEquals) {
            result.add(member);
          }
        }
      }
      _xblockexpression = result;
    }
    return ((IResource[])Conversions.unwrapArray(_xblockexpression, IResource.class));
  }
}
