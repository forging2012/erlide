package org.erlide.engine.new_model;

import org.eclipse.handly.model.ISourceFile;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlForm;

@SuppressWarnings("all")
public interface IErlSource extends IErlElement, ISourceFile {
  public abstract Iterable<IErlForm> getForms();
  
  public abstract String getExtension();
}
