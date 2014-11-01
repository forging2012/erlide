package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlExpression;
import org.erlide.engine.new_model.IErlForm;

@SuppressWarnings("all")
public interface IErlAttribute extends IErlForm {
  public abstract Iterable<IErlExpression> getValues();
}
