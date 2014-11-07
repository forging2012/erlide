package org.erlide.engine.new_model.internal;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.handly.model.IElementChangeEvent;
import org.eclipse.handly.model.IElementChangeListener;
import org.eclipse.handly.model.impl.ElementChangeEvent;
import org.eclipse.handly.model.impl.HandleDelta;
import org.eclipse.handly.model.impl.HandleManager;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlModelManager;
import org.erlide.engine.new_model.internal.ErlDeltaProcessor;
import org.erlide.engine.new_model.internal.ErlModel;
import org.erlide.engine.new_model.internal.ErlModelCache;
import org.erlide.util.ErlLogger;

/**
 * The manager for the Erlang model.
 * 
 * @threadsafe This class is intended to be thread-safe
 */
@SuppressWarnings("all")
public class ErlModelManager implements IErlModelManager, IResourceChangeListener {
  public final static ErlModelManager INSTANCE = new ErlModelManager();
  
  private IErlModel erlModel;
  
  private HandleManager handleManager;
  
  private ListenerList listenerList;
  
  public void startup() {
    ErlModel _erlModel = new ErlModel();
    this.erlModel = _erlModel;
    ErlModelCache _erlModelCache = new ErlModelCache();
    HandleManager _handleManager = new HandleManager(_erlModelCache);
    this.handleManager = _handleManager;
    ListenerList _listenerList = new ListenerList();
    this.listenerList = _listenerList;
    IWorkspace _workspace = this.erlModel.getWorkspace();
    _workspace.addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE);
  }
  
  public void shutdown() {
    IWorkspace _workspace = this.erlModel.getWorkspace();
    _workspace.removeResourceChangeListener(this);
    this.listenerList = null;
    this.handleManager = null;
    this.erlModel = null;
  }
  
  public IErlModel getErlModel() {
    boolean _tripleEquals = (this.erlModel == null);
    if (_tripleEquals) {
      throw new IllegalStateException();
    }
    return this.erlModel;
  }
  
  public HandleManager getHandleManager() {
    boolean _tripleEquals = (this.handleManager == null);
    if (_tripleEquals) {
      throw new IllegalStateException();
    }
    return this.handleManager;
  }
  
  private ErlModelManager() {
  }
  
  public void resourceChanged(final IResourceChangeEvent event) {
    int _type = event.getType();
    boolean _notEquals = (_type != IResourceChangeEvent.POST_CHANGE);
    if (_notEquals) {
      return;
    }
    final ErlDeltaProcessor deltaProcessor = new ErlDeltaProcessor();
    try {
      IResourceDelta _delta = event.getDelta();
      _delta.accept(deltaProcessor);
    } catch (final Throwable _t) {
      if (_t instanceof CoreException) {
        final CoreException e = (CoreException)_t;
        ErlLogger.error(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    HandleDelta _delta_1 = deltaProcessor.getDelta();
    boolean _isEmpty = _delta_1.isEmpty();
    boolean _not = (!_isEmpty);
    if (_not) {
      HandleDelta _delta_2 = deltaProcessor.getDelta();
      ElementChangeEvent _elementChangeEvent = new ElementChangeEvent(ElementChangeEvent.POST_CHANGE, _delta_2);
      this.fireElementChangeEvent(_elementChangeEvent);
    }
  }
  
  public void addElementChangeListener(final IElementChangeListener listener) {
    boolean _tripleEquals = (this.listenerList == null);
    if (_tripleEquals) {
      throw new IllegalStateException();
    }
    this.listenerList.add(listener);
  }
  
  public void removeElementChangeListener(final IElementChangeListener listener) {
    boolean _tripleEquals = (this.listenerList == null);
    if (_tripleEquals) {
      throw new IllegalStateException();
    }
    this.listenerList.remove(listener);
  }
  
  public void fireElementChangeEvent(final IElementChangeEvent event) {
    boolean _tripleEquals = (this.listenerList == null);
    if (_tripleEquals) {
      throw new IllegalStateException();
    }
    final Object[] listeners = this.listenerList.getListeners();
    for (final Object listener : listeners) {
      SafeRunner.run(
        new ISafeRunnable() {
          public void handleException(final Throwable exception) {
          }
          
          public void run() {
            ((IElementChangeListener) listener).elementChanged(event);
          }
        });
    }
  }
}
