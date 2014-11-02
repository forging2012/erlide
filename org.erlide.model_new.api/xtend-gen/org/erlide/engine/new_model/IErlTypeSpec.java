package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlExpression;
import org.erlide.engine.new_model.IErlFunction;

@SuppressWarnings("all")
public interface IErlTypeSpec extends IErlAttribute {
  public abstract IErlFunction getFunction();
  
  public abstract IErlExpression getSpec();
}
