package org.erlide.engine.new_model;

import com.google.common.base.Objects;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.engine.new_model.internal.ErlModelManager;

@SuppressWarnings("all")
public class ErlModelCore {
  public static IErlModel getErlModel() {
    return ErlModelManager.INSTANCE.getErlModel();
  }
  
  public static IErlProject create(final IProject project) {
    IErlProject _xifexpression = null;
    boolean _tripleEquals = (project == null);
    if (_tripleEquals) {
      _xifexpression = null;
    } else {
      IErlModel _erlModel = ErlModelCore.getErlModel();
      String _name = project.getName();
      _xifexpression = _erlModel.getProject(_name);
    }
    return _xifexpression;
  }
  
  public static IErlSource create(final IFile file) {
    IErlSource _xblockexpression = null;
    {
      boolean _tripleEquals = (file == null);
      if (_tripleEquals) {
        return null;
      }
      IContainer _parent = file.getParent();
      int _type = _parent.getType();
      boolean _notEquals = (_type != IResource.PROJECT);
      if (_notEquals) {
        return null;
      }
      IProject _project = file.getProject();
      final IErlProject project = ErlModelCore.create(_project);
      String _name = file.getName();
      _xblockexpression = project.getSourceFile(_name);
    }
    return _xblockexpression;
  }
  
  /**
   * Returns the Erlang element corresponding to the given resource, or
   * <code>null</code> if unable to associate the given resource
   * with an element of the Erlang Model.
   * 
   * @param resource the given resource (maybe <code>null</code>)
   * @return the Erlang element corresponding to the given resource, or
   *  <code>null</code> if unable to associate the given resource
   *  with an element of the Erlang Model
   */
  public static IErlElement create(final IResource resource) {
    IErlElement _xblockexpression = null;
    {
      boolean _equals = Objects.equal(resource, null);
      if (_equals) {
        return null;
      }
      final int type = resource.getType();
      IErlElement _switchResult = null;
      switch (type) {
        case IResource.PROJECT:
          _switchResult = ErlModelCore.create(((IProject) resource));
          break;
        case IResource.FILE:
          _switchResult = ErlModelCore.create(((IFile) resource));
          break;
        case IResource.ROOT:
          _switchResult = ErlModelCore.getErlModel();
          break;
        default:
          _switchResult = null;
          break;
      }
      _xblockexpression = _switchResult;
    }
    return _xblockexpression;
  }
  
  private ErlModelCore() {
  }
}
