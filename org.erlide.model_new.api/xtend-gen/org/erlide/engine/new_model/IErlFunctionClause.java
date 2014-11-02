package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlExpression;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlGuard;

@SuppressWarnings("all")
public interface IErlFunctionClause extends IErlForm {
  public abstract Iterable<IErlExpression> getFormalParameters();
  
  public abstract IErlGuard getGuard();
  
  public abstract IErlExpression getFunctionBody();
}
