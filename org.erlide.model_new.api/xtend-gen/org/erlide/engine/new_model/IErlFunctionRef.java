package org.erlide.engine.new_model;

@SuppressWarnings("all")
public interface IErlFunctionRef {
  public abstract String getModule();
  
  public abstract String getName();
  
  public abstract int getArity();
}
