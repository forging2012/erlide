package org.erlide.ui.editors.erl;

import org.eclipse.core.resources.IFile;
import org.eclipse.handly.model.ISourceFile;
import org.eclipse.handly.model.ISourceFileFactory;
import org.erlide.engine.new_model.ErlModelCore;

@SuppressWarnings("all")
public class ErlFileFactory implements ISourceFileFactory {
  public ISourceFile getSourceFile(final IFile file) {
    return ErlModelCore.create(file);
  }
}
