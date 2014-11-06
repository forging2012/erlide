package org.erlide.engine.new_model;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.handly.model.IElementChangeListener;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlProject;

@SuppressWarnings("all")
public interface IErlModel extends IErlElement {
  public abstract IWorkspace getWorkspace();
  
  public abstract Iterable<IErlProject> getProjects();
  
  public abstract IErlProject getProject(final String name);
  
  /**
   * Adds the given listener for changes to elements in the Erlang Model.
   * Has no effect if an identical listener is already registered.
   * <p>
   * Once registered, a listener starts receiving notification
   * of changes to elements in the Erlang Model. The listener continues
   * to receive notifications until it is removed.
   * </p>
   * 
   * @param listener the listener (not <code>null</code>)
   * @see #removeElementChangeListener(IElementChangeListener)
   */
  public abstract void addElementChangeListener(final IElementChangeListener listener);
  
  /**
   * Removes the given element change listener.
   * Has no effect if an identical listener is not registered.
   * 
   * @param listener the listener (not <code>null</code>)
   */
  public abstract void removeElementChangeListener(final IElementChangeListener listener);
}
