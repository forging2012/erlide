package org.erlide.engine.new_model.internal;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.Handle;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlProject;

/**
 * This class is used by the <code>FooModelManager</code> to process
 * resource deltas and update the Foo Model accordingly.
 */
@SuppressWarnings("all")
public class ErlDeltaProcessor implements IResourceDeltaVisitor {
  public boolean visit(final IResourceDelta delta) throws CoreException {
    IResource _resource = delta.getResource();
    int _type = _resource.getType();
    switch (_type) {
      case IResource.ROOT:
        return this.processRoot(delta);
      case IResource.PROJECT:
        return this.processProject(delta);
      default:
        return true;
    }
  }
  
  private boolean processRoot(final IResourceDelta delta) throws CoreException {
    return true;
  }
  
  private boolean processProject(final IResourceDelta delta) throws CoreException {
    int _kind = delta.getKind();
    switch (_kind) {
      case IResourceDelta.ADDED:
        return this.processAddedProject(delta);
      case IResourceDelta.REMOVED:
        return this.processRemovedProject(delta);
      default:
        return true;
    }
  }
  
  private boolean processAddedProject(final IResourceDelta delta) throws CoreException {
    IResource _resource = delta.getResource();
    final IProject project = ((IProject) _resource);
    boolean _hasNature = project.hasNature(IErlProject.NATURE_ID);
    if (_hasNature) {
      final IErlProject erlProject = ErlModelCore.create(project);
      ErlDeltaProcessor.addToModel(erlProject);
    }
    return false;
  }
  
  private boolean processRemovedProject(final IResourceDelta delta) throws CoreException {
    IResource _resource = delta.getResource();
    final IProject project = ((IProject) _resource);
    final IErlProject erlProject = ErlModelCore.create(project);
    ErlDeltaProcessor.removeFromModel(erlProject);
    return false;
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
