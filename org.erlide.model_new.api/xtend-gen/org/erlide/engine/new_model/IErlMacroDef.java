package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlExpression;

@SuppressWarnings("all")
public interface IErlMacroDef extends IErlAttribute {
  public abstract IErlExpression getValue();
}
