package org.erlide.engine.new_model;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.handly.model.ISourceFile;
import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlComment;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlError;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlFunction;

@SuppressWarnings("all")
public interface IErlSource extends IErlElement, ISourceFile {
  public abstract Iterable<IErlForm> getForms() throws CoreException;
  
  public abstract String getExtension();
  
  public abstract IErlComment getHeaderComment();
  
  public abstract Iterable<IErlAttribute> getAttributes() throws CoreException;
  
  public abstract Iterable<IErlAttribute> getAttributesWithTag(final String tag);
  
  public abstract Iterable<IErlFunction> getFunctions() throws CoreException;
  
  public abstract IErlFunction getFunction(final String name, final int arity);
  
  public abstract Iterable<IErlError> getErrors() throws CoreException;
}
