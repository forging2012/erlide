package org.erlide.engine.new_model.internal;

import com.google.common.base.Objects;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.IHandleDelta;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.Handle;
import org.eclipse.handly.model.impl.HandleDelta;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;

/**
 * This class is used by the <code>ErlModelManager</code> to process
 * resource deltas and update the Erl Model accordingly.
 */
@SuppressWarnings("all")
public class ErlDeltaProcessor implements IResourceDeltaVisitor {
  private final HandleDelta currentDelta = new HandleDelta(ErlModelCore.getErlModel());
  
  private final Set<String> oldErlProjectNames = new HashSet<String>();
  
  /**
   * Returns the Erl element delta built from the resource delta.
   * Returns an empty delta if no Erl elements were affected
   * by the resource change.
   * 
   * @return Erl element delta (never <code>null</code>)
   */
  public HandleDelta getDelta() {
    return this.currentDelta;
  }
  
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
    boolean _xblockexpression = false;
    {
      this.initOldErlProjectNames();
      _xblockexpression = true;
    }
    return _xblockexpression;
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
      case IResourceDelta.CHANGED:
        return this.processChangedProject(delta);
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
          this.translateAddedDelta(delta, erlProject);
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
      this.translateRemovedDelta(delta, erlProject);
      _xblockexpression = false;
    }
    return _xblockexpression;
  }
  
  private boolean processChangedProject(final IResourceDelta delta) {
    try {
      IResource _resource = delta.getResource();
      final IProject project = ((IProject) _resource);
      final IErlProject erlProject = ErlModelCore.create(project);
      int _flags = delta.getFlags();
      int _bitwiseAnd = (_flags & IResourceDelta.OPEN);
      boolean _notEquals = (_bitwiseAnd != 0);
      if (_notEquals) {
        boolean _isOpen = project.isOpen();
        if (_isOpen) {
          boolean _hasNature = project.hasNature(IErlProject.NATURE_ID);
          if (_hasNature) {
            ErlDeltaProcessor.addToModel(erlProject);
            this.currentDelta.insertAdded(erlProject, IHandleDelta.F_OPEN);
          }
        } else {
          boolean _wasErlProject = this.wasErlProject(project);
          if (_wasErlProject) {
            ErlDeltaProcessor.removeFromModel(erlProject);
            this.currentDelta.insertRemoved(erlProject, IHandleDelta.F_OPEN);
          }
        }
        return false;
      }
      final boolean isErlProject = project.hasNature(IErlProject.NATURE_ID);
      int _flags_1 = delta.getFlags();
      int _bitwiseAnd_1 = (_flags_1 & IResourceDelta.DESCRIPTION);
      boolean _notEquals_1 = (_bitwiseAnd_1 != 0);
      if (_notEquals_1) {
        final boolean wasErlProject = this.wasErlProject(project);
        if ((wasErlProject != isErlProject)) {
          if (isErlProject) {
            ErlDeltaProcessor.addToModel(erlProject);
            this.currentDelta.insertAdded(erlProject, IHandleDelta.F_DESCRIPTION);
          } else {
            ErlDeltaProcessor.removeFromModel(erlProject);
            this.currentDelta.insertRemoved(erlProject, IHandleDelta.F_DESCRIPTION);
          }
          return false;
        } else {
          if (isErlProject) {
            this.currentDelta.insertChanged(erlProject, IHandleDelta.F_DESCRIPTION);
          }
        }
      }
      if (isErlProject) {
        IHandle _parent = erlProject.getParent();
        final Body parentBody = ErlDeltaProcessor.findBody(_parent);
        final IHandle[] children = parentBody.getChildren();
        boolean _contains = ((List<IHandle>)Conversions.doWrapArray(children)).contains(erlProject);
        boolean _not = (!_contains);
        if (_not) {
          ErlDeltaProcessor.addToModel(erlProject);
        }
        return true;
      } else {
        return false;
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
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
  
  private boolean processAddedFile(final IResourceDelta delta) {
    boolean _xblockexpression = false;
    {
      IResource _resource = delta.getResource();
      final IFile file = ((IFile) _resource);
      final IErlSource erlFile = ErlModelCore.create(file);
      boolean _notEquals = (!Objects.equal(erlFile, null));
      if (_notEquals) {
        ErlDeltaProcessor.addToModel(erlFile);
        this.translateAddedDelta(delta, erlFile);
      }
      _xblockexpression = false;
    }
    return _xblockexpression;
  }
  
  private boolean processRemovedFile(final IResourceDelta delta) {
    boolean _xblockexpression = false;
    {
      IResource _resource = delta.getResource();
      final IFile file = ((IFile) _resource);
      final IErlSource erlFile = ErlModelCore.create(file);
      boolean _notEquals = (!Objects.equal(erlFile, null));
      if (_notEquals) {
        ErlDeltaProcessor.removeFromModel(erlFile);
        this.translateRemovedDelta(delta, erlFile);
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
  
  public HandleDelta contentChanged(final IErlSource source) {
    HandleDelta _xblockexpression = null;
    {
      ErlDeltaProcessor.close(source);
      _xblockexpression = this.currentDelta.insertChanged(source, IHandleDelta.F_CONTENT);
    }
    return _xblockexpression;
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
  
  private void translateAddedDelta(final IResourceDelta delta, final IErlElement element) {
    int _flags = delta.getFlags();
    int _bitwiseAnd = (_flags & IResourceDelta.MOVED_FROM);
    boolean _equals = (_bitwiseAnd == 0);
    if (_equals) {
      this.currentDelta.insertAdded(element);
    } else {
      IPath _movedFromPath = delta.getMovedFromPath();
      IResource _resource = delta.getResource();
      int _type = _resource.getType();
      IResource _resource_1 = ErlDeltaProcessor.getResource(_movedFromPath, _type);
      final IErlElement movedFromElement = ErlModelCore.create(_resource_1);
      boolean _equals_1 = Objects.equal(movedFromElement, null);
      if (_equals_1) {
        this.currentDelta.insertAdded(element);
      } else {
        this.currentDelta.insertMovedTo(element, movedFromElement);
      }
    }
  }
  
  private void translateRemovedDelta(final IResourceDelta delta, final IErlElement element) {
    int _flags = delta.getFlags();
    int _bitwiseAnd = (_flags & IResourceDelta.MOVED_TO);
    boolean _equals = (_bitwiseAnd == 0);
    if (_equals) {
      this.currentDelta.insertRemoved(element);
    } else {
      IPath _movedToPath = delta.getMovedToPath();
      IResource _resource = delta.getResource();
      int _type = _resource.getType();
      IResource _resource_1 = ErlDeltaProcessor.getResource(_movedToPath, _type);
      final IErlElement movedToElement = ErlModelCore.create(_resource_1);
      boolean _equals_1 = Objects.equal(movedToElement, null);
      if (_equals_1) {
        this.currentDelta.insertRemoved(element);
      } else {
        this.currentDelta.insertMovedFrom(element, movedToElement);
      }
    }
  }
  
  private static IResource getResource(final IPath fullPath, final int resourceType) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IWorkspaceRoot root = _workspace.getRoot();
    switch (resourceType) {
      case IResource.ROOT:
        return root;
      case IResource.PROJECT:
        String _lastSegment = fullPath.lastSegment();
        return root.getProject(_lastSegment);
      case IResource.FOLDER:
        return root.getFolder(fullPath);
      case IResource.FILE:
        return root.getFile(fullPath);
      default:
        return null;
    }
  }
  
  private void initOldErlProjectNames() {
    IErlModel _erlModel = ErlModelCore.getErlModel();
    final Iterable<IErlProject> erlProjects = _erlModel.getProjects();
    for (final IErlProject erlProject : erlProjects) {
      String _name = erlProject.getName();
      this.oldErlProjectNames.add(_name);
    }
  }
  
  private boolean wasErlProject(final IProject project) {
    String _name = project.getName();
    return this.oldErlProjectNames.contains(_name);
  }
}
