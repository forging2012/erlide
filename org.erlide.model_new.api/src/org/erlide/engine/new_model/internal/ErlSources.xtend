package org.erlide.engine.new_model.internal

import org.eclipse.core.resources.IFile
import org.eclipse.xtend.lib.annotations.Data
import org.erlide.engine.new_model.IErlHeader
import org.erlide.engine.new_model.IErlModule

@Data
class ErlModule extends ErlSource implements IErlModule {

  new(ErlProject parent, IFile file) {
    super(parent, file)
    if (!extension.equals(file.fileExtension))
      throw new IllegalArgumentException()
  }

  override getExtension() {
    "erl"
  }

}

@Data
class ErlHeader extends ErlSource implements IErlHeader {

  new(ErlProject parent, IFile file) {
    super(parent, file)
    if (!extension.equals(file.fileExtension))
      throw new IllegalArgumentException()
  }

  override getExtension() {
    "hrl"
  }

}

