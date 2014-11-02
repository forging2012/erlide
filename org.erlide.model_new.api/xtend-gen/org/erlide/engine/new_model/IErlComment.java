package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlForm;

@SuppressWarnings("all")
public interface IErlComment extends IErlForm {
  public abstract String getValue();
  
  public abstract int getLevel();
}
