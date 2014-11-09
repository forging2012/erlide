package org.erlide.engine.new_model.internal

import java.util.Map
import org.eclipse.core.runtime.CoreException
import org.eclipse.handly.model.IHandle
import org.eclipse.handly.model.impl.Body
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.model.root.ErlangLibraryProperties
import org.erlide.engine.new_model.IErlLibrary

@Data
class ErlLibrary extends ErlElement implements IErlLibrary {

  ErlangLibraryProperties properties

  new(ErlModel parent, String name, ErlangLibraryProperties properties) {
    super(parent, name)
    if (parent === null)
      throw new IllegalArgumentException()
    this.properties = properties
  }

  override protected buildStructure(Body body, Map<IHandle, Body> newElements) throws CoreException {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

  override protected validateExistence() throws CoreException {
    // nothing for now
  }

  override getResource() {
    null
  }

  override getFolders() {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

  override getSourceFolders() {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

  override getIncludeFolders() {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

  override getBinaryFolder() {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

  override getLibraries() {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

  override getNonErlangResources() {
    throw new UnsupportedOperationException("auto-generated method stub")

  //TODO: auto-generated method stub
  }

}
