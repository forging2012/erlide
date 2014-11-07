package org.erlide.ui.editors.erl

import org.eclipse.core.resources.IFile
import org.eclipse.handly.model.ISourceFile
import org.eclipse.handly.model.ISourceFileFactory
import org.erlide.engine.new_model.ErlModelCore

class ErlFileFactory implements ISourceFileFactory {

    override ISourceFile getSourceFile(IFile file) {
        return ErlModelCore.create(file);
    }

}
