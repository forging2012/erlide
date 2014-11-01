package org.erlide.engine.new_model;

import org.eclipse.handly.model.impl.HandleManager;
import org.erlide.engine.new_model.IErlModel;

@SuppressWarnings("all")
public interface IErlModelManager {
  public abstract void startup();
  
  public abstract void shutdown();
  
  public abstract IErlModel getErlModel();
  
  public abstract HandleManager getHandleManager();
}
