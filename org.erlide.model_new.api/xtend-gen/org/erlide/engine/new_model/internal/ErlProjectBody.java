package org.erlide.engine.new_model.internal;

import java.util.List;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;

@SuppressWarnings("all")
public class ErlProjectBody extends Body {
  private IResource[] nonErlResources;
  
  public Iterable<IResource> getNonErlResources(final IErlProject erlProject) throws CoreException {
    IResource[] _xblockexpression = null;
    {
      boolean _tripleEquals = (this.nonErlResources == null);
      if (_tripleEquals) {
        final IProject resource = erlProject.getWorkspaceProject();
        List<IResource> _computeNonErlResources = this.computeNonErlResources(resource);
        this.nonErlResources = ((IResource[])Conversions.unwrapArray(_computeNonErlResources, IResource.class));
      }
      _xblockexpression = this.nonErlResources;
    }
    return (Iterable<IResource>)Conversions.doWrapArray(_xblockexpression);
  }
  
  public void setNonErlResources(final IResource[] resources) {
    this.nonErlResources = resources;
  }
  
  private List<IResource> computeNonErlResources(final IContainer resource) throws CoreException {
    List<IResource> _xblockexpression = null;
    {
      final List<IResource> result = CollectionLiterals.<IResource>newArrayList();
      final IResource[] members = resource.members();
      for (final IResource member : members) {
        if ((member instanceof IFile)) {
          IErlSource _create = ErlModelCore.create(((IFile)member));
          boolean _tripleEquals = (_create == null);
          if (_tripleEquals) {
            result.add(member);
          }
        } else {
          if ((member instanceof IContainer)) {
            List<IResource> _computeNonErlResources = this.computeNonErlResources(((IContainer)member));
            result.addAll(_computeNonErlResources);
          }
        }
      }
      _xblockexpression = result;
    }
    return _xblockexpression;
  }
}
