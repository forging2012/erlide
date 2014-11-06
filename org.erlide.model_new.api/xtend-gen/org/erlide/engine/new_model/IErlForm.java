package org.erlide.engine.new_model;

import org.eclipse.handly.model.ISourceConstruct;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlModule;

@SuppressWarnings("all")
public interface IErlForm extends IErlElement, ISourceConstruct {
  public abstract IErlModule getModule();
}
