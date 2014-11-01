package org.erlide.engine.new_model;

import java.util.Map;
import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlFunctionRef;

@SuppressWarnings("all")
public interface IErlImport extends IErlAttribute {
  public abstract Map<String, IErlFunctionRef> getFunctions();
}
