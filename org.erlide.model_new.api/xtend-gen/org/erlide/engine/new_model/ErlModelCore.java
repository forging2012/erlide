package org.erlide.engine.new_model;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.new_model.IErlModel;
import org.erlide.engine.new_model.IErlProject;
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
  
  private ErlModelCore() {
  }
}
