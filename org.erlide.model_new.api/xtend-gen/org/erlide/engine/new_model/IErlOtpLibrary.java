package org.erlide.engine.new_model;

import org.erlide.engine.new_model.IErlLibrary;

@SuppressWarnings("all")
public interface IErlOtpLibrary extends IErlLibrary {
  public abstract String getVersion();
}
