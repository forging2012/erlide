package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlComment;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlFunctionClause;
import org.erlide.engine.new_model.IErlTypeSpec;

@SuppressWarnings("all")
public interface IErlFunction extends IErlForm {
  public abstract int getArity();
  
  public abstract Iterable<IErlFunctionClause> getClauses();
  
  public abstract IErlTypeSpec getTypeSpecification();
  
  public abstract IErlComment getComment();
}
