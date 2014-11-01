package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlLibraryContainer;

@SuppressWarnings("all")
public interface IErlOtpLibrary extends IErlLibraryContainer {
  public abstract String getVersion();
}
