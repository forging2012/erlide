package org.erlide.engine.new_model;

import org.eclipse.handly.model.ISourceConstruct;
import org.erlide.engine.new_model.IErlElement;

@SuppressWarnings("all")
public interface IErlExpression extends IErlElement, ISourceConstruct {
  public abstract String getContent();
}
