package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlForm;

@SuppressWarnings("all")
public interface IErlIf extends IErlAttribute {
  public abstract Iterable<IErlForm> getIfForms();
  
  public abstract Iterable<IErlForm> getElseForms();
}
