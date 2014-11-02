package org.erlide.engine.new_model.internal;

import java.util.HashMap;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.IBodyCache;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;

@SuppressWarnings("all")
public class ErlModelCache implements IBodyCache {
  private final static int DEFAULT_PROJECT_SIZE = 5;
  
  private Body modelBody;
  
  private final HashMap<IHandle, Body> projectCache;
  
  public ErlModelCache() {
    HashMap<IHandle, Body> _hashMap = new HashMap<IHandle, Body>(ErlModelCache.DEFAULT_PROJECT_SIZE);
    this.projectCache = _hashMap;
  }
  
  public Body get(final IHandle handle) {
    Body _xifexpression = null;
    if ((handle instanceof IErlModel)) {
      _xifexpression = this.modelBody;
    } else {
      Body _xifexpression_1 = null;
      if ((handle instanceof IErlProject)) {
        _xifexpression_1 = this.projectCache.get(handle);
      } else {
        _xifexpression_1 = null;
      }
      _xifexpression = _xifexpression_1;
    }
    return _xifexpression;
  }
  
  public Body peek(final IHandle handle) {
    Body _xifexpression = null;
    if ((handle instanceof IErlModel)) {
      _xifexpression = this.modelBody;
    } else {
      Body _xifexpression_1 = null;
      if ((handle instanceof IErlProject)) {
        _xifexpression_1 = this.projectCache.get(handle);
      } else {
        _xifexpression_1 = null;
      }
      _xifexpression = _xifexpression_1;
    }
    return _xifexpression;
  }
  
  public void put(final IHandle handle, final Body body) {
    if ((handle instanceof IErlModel)) {
      this.modelBody = body;
    } else {
      if ((handle instanceof IErlProject)) {
        this.projectCache.put(handle, body);
      }
    }
  }
  
  public void remove(final IHandle handle) {
    if ((handle instanceof IErlModel)) {
      this.modelBody = null;
    } else {
      if ((handle instanceof IErlProject)) {
        this.projectCache.remove(handle);
      }
    }
  }
}
