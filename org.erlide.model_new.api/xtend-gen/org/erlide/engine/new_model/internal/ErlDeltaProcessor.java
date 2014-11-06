package org.erlide.engine.new_model.internal;

import com.google.common.base.Objects;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.Handle;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;

/**
 * This class is used by the <code>FooModelManager</code> to process
 * resource deltas and update the Foo Model accordingly.
 */
@SuppressWarnings("all")
public class ErlDeltaProcessor implements IResourceDeltaVisitor {
  public boolean visit(final IResourceDelta delta) {
    boolean _switchResult = false;
    IResource _resource = delta.getResource();
    int _type = _resource.getType();
    switch (_type) {
      case IResource.ROOT:
        _switchResult = this.processRoot(delta);
        break;
      case IResource.PROJECT:
        _switchResult = this.processProject(delta);
        break;
      case IResource.FILE:
        _switchResult = this.processFile(delta);
        break;
      default:
        _switchResult = true;
        break;
    }
    return _switchResult;
  }
  
  private boolean processRoot(final IResourceDelta delta) {
    return true;
  }
  
  private boolean processProject(final IResourceDelta delta) {
    boolean _switchResult = false;
    int _kind = delta.getKind();
    switch (_kind) {
      case IResourceDelta.ADDED:
        _switchResult = this.processAddedProject(delta);
        break;
      case IResourceDelta.REMOVED:
        _switchResult = this.processRemovedProject(delta);
        break;
      default:
        _switchResult = true;
        break;
    }
    return _switchResult;
  }
  
  private boolean processAddedProject(final IResourceDelta delta) {
    try {
      boolean _xblockexpression = false;
      {
        IResource _resource = delta.getResource();
        final IProject project = ((IProject) _resource);
        boolean _and = false;
        boolean _isOpen = project.isOpen();
        if (!_isOpen) {
          _and = false;
        } else {
          boolean _hasNature = project.hasNature(IErlProject.NATURE_ID);
          _and = _hasNature;
        }
        if (_and) {
          final IErlProject erlProject = ErlModelCore.create(project);
          ErlDeltaProcessor.addToModel(erlProject);
        }
        _xblockexpression = false;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  private boolean processRemovedProject(final IResourceDelta delta) {
    boolean _xblockexpression = false;
    {
      IResource _resource = delta.getResource();
      final IProject project = ((IProject) _resource);
      final IErlProject erlProject = ErlModelCore.create(project);
      ErlDeltaProcessor.removeFromModel(erlProject);
      _xblockexpression = false;
    }
    return _xblockexpression;
  }
  
  private boolean processFile(final IResourceDelta delta) {
    boolean _switchResult = false;
    int _kind = delta.getKind();
    switch (_kind) {
      case IResourceDelta.ADDED:
        _switchResult = this.processAddedFile(delta);
        break;
      case IResourceDelta.REMOVED:
        _switchResult = this.processRemovedFile(delta);
        break;
      case IResourceDelta.CHANGED:
        _switchResult = this.processChangedFile(delta);
        break;
      default:
        _switchResult = true;
        break;
    }
    return _switchResult;
  }
  
  public boolean processAddedFile(final IResourceDelta delta) {
    boolean _xblockexpression = false;
    {
      IResource _resource = delta.getResource();
      final IFile file = ((IFile) _resource);
      final IErlSource erlFile = ErlModelCore.create(file);
      boolean _notEquals = (!Objects.equal(erlFile, null));
      if (_notEquals) {
        ErlDeltaProcessor.addToModel(erlFile);
      }
      _xblockexpression = false;
    }
    return _xblockexpression;
  }
  
  public boolean processRemovedFile(final IResourceDelta delta) {
    boolean _xblockexpression = false;
    {
      IResource _resource = delta.getResource();
      final IFile file = ((IFile) _resource);
      final IErlSource erlFile = ErlModelCore.create(file);
      boolean _notEquals = (!Objects.equal(erlFile, null));
      if (_notEquals) {
        ErlDeltaProcessor.removeFromModel(erlFile);
      }
      _xblockexpression = false;
    }
    return _xblockexpression;
  }
  
  public boolean processChangedFile(final IResourceDelta delta) {
    IResource _resource = delta.getResource();
    final IFile file = ((IFile) _resource);
    final IErlSource erlFile = ErlModelCore.create(file);
    boolean _notEquals = (!Objects.equal(erlFile, null));
    if (_notEquals) {
      int _flags = delta.getFlags();
      int _bitwiseOr = (IResourceDelta.MARKERS | IResourceDelta.SYNC);
      int _bitwiseAnd = (_flags & _bitwiseOr);
      int _bitwiseNot = (~_bitwiseAnd);
      boolean _notEquals_1 = (_bitwiseNot != 0);
      if (_notEquals_1) {
        this.contentChanged(erlFile);
      }
    }
    return false;
  }
  
  public void contentChanged(final IErlSource source) {
    ErlDeltaProcessor.close(source);
  }
  
  private static void addToModel(final IHandle element) {
    IHandle _parent = element.getParent();
    final Body parentBody = ErlDeltaProcessor.findBody(_parent);
    boolean _tripleNotEquals = (parentBody != null);
    if (_tripleNotEquals) {
      parentBody.addChild(element);
    }
    ErlDeltaProcessor.close(element);
  }
  
  private static void removeFromModel(final IHandle element) {
    IHandle _parent = element.getParent();
    final Body parentBody = ErlDeltaProcessor.findBody(_parent);
    boolean _tripleNotEquals = (parentBody != null);
    if (_tripleNotEquals) {
      parentBody.removeChild(element);
    }
    ErlDeltaProcessor.close(element);
  }
  
  private static Body findBody(final IHandle element) {
    return ((Handle) element).findBody();
  }
  
  private static void close(final IHandle element) {
    ((Handle) element).close();
  }
}
