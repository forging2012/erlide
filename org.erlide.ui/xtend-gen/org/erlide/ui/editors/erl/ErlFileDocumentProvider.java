package org.erlide.ui.editors.erl;

import org.eclipse.handly.ui.texteditor.SourceFileDocumentProvider;
import org.erlide.ui.editors.erl.ErlFileFactory;

@SuppressWarnings("all")
public class ErlFileDocumentProvider extends SourceFileDocumentProvider {
  public ErlFileDocumentProvider() {
    super(new ErlFileFactory());
  }
}
