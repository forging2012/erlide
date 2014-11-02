package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlElement;
import org.erlide.engine.new_model.IErlLibrary;

@SuppressWarnings("all")
public interface IErlLibraryContainer extends IErlElement {
  public abstract Iterable<IErlLibrary> getLibraries();
}
