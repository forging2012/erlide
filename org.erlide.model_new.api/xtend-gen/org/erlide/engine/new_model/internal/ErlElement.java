package org.erlide.engine.new_model.internal;

import org.eclipse.handly.model.impl.Handle;
import org.eclipse.handly.model.impl.HandleManager;
import org.erlide.engine.new_model.internal.ErlModelManager;

@SuppressWarnings("all")
public abstract class ErlElement extends Handle {
  public ErlElement(final Handle parent, final String name) {
    super(parent, name);
  }
  
  protected HandleManager getHandleManager() {
    return ErlModelManager.INSTANCE.getHandleManager();
  }
}
