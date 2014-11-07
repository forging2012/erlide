package org.erlide.ui.editors.erl

import org.eclipse.handly.ui.texteditor.SourceFileDocumentProvider

class ErlFileDocumentProvider extends SourceFileDocumentProvider {

    new() {
        super(new ErlFileFactory());
    }

}
