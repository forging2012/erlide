package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlFunctionRef;

@SuppressWarnings("all")
public interface IErlExport extends IErlAttribute {
  public abstract Iterable<IErlFunctionRef> getFunctions();
}
