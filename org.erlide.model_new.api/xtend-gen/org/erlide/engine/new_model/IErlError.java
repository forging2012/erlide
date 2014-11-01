package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlForm;

@SuppressWarnings("all")
public interface IErlError extends IErlForm {
  public abstract String getMessage();
}
