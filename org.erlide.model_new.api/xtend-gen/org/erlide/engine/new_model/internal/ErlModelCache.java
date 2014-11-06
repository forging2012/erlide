package org.erlide.engine.new_model.internal;

import java.util.HashMap;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.ElementCache;
import org.eclipse.handly.model.impl.IBodyCache;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
import org.erlide.engine.new_model.IErlSource;

@SuppressWarnings("all")
public class ErlModelCache implements IBodyCache {
  private final static int DEFAULT_PROJECT_SIZE = 5;
  
  private final static int DEFAULT_FILE_SIZE = 100;
  
  private final static int DEFAULT_CHILDREN_SIZE = (ErlModelCache.DEFAULT_FILE_SIZE * 20);
  
  private Body modelBody;
  
  private final HashMap<IHandle, Body> projectCache;
  
  private ElementCache fileCache;
  
  private HashMap<IHandle, Body> childrenCache;
  
  public ErlModelCache() {
    HashMap<IHandle, Body> _hashMap = new HashMap<IHandle, Body>(ErlModelCache.DEFAULT_PROJECT_SIZE);
    this.projectCache = _hashMap;
    ElementCache _elementCache = new ElementCache(ErlModelCache.DEFAULT_FILE_SIZE);
    this.fileCache = _elementCache;
    HashMap<IHandle, Body> _hashMap_1 = new HashMap<IHandle, Body>(ErlModelCache.DEFAULT_CHILDREN_SIZE);
    this.childrenCache = _hashMap_1;
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
        if ((handle instanceof IErlSource)) {
          return this.fileCache.get(handle);
        } else {
          return this.childrenCache.get(handle);
        }
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
        if ((handle instanceof IErlSource)) {
          return this.fileCache.peek(handle);
        } else {
          return this.childrenCache.get(handle);
        }
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
        this.fileCache.ensureSpaceLimit(body, handle);
      } else {
        if ((handle instanceof IErlSource)) {
          this.fileCache.put(handle, body);
        } else {
          this.childrenCache.put(handle, body);
        }
      }
    }
  }
  
  public void remove(final IHandle handle) {
    if ((handle instanceof IErlModel)) {
      this.modelBody = null;
    } else {
      if ((handle instanceof IErlProject)) {
        this.projectCache.remove(handle);
        this.fileCache.resetSpaceLimit(ErlModelCache.DEFAULT_FILE_SIZE, handle);
      } else {
        if ((handle instanceof IErlSource)) {
          this.fileCache.remove(handle);
        } else {
          this.childrenCache.remove(handle);
        }
      }
    }
  }
}
