/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.internal.reconciling;

// import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.handly.model.ISourceFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlFileDocumentProvider;
import org.erlide.util.ErlLogger;

public class ErlReconcilingStrategy implements IErlReconcilingStrategy,
        IReconcilingStrategyExtension {

    private IErlModule module;
    private final AbstractErlangEditor editor;
    private final ErlFileDocumentProvider documentProvider;
    // private IDocument fDoc;
    private IProgressMonitor mon;
    private ScannerService scanner;

    // private boolean initialInsert;

    public ErlReconcilingStrategy(final AbstractErlangEditor editor) {
        this.editor = editor;
        if (editor.getDocumentProvider() instanceof ErlFileDocumentProvider) {
            documentProvider = (ErlFileDocumentProvider) editor.getDocumentProvider();
        } else {
            documentProvider = null;
        }
    }

    @Override
    public void setDocument(final IDocument document) {
        if (editor == null) {
            return;
        }
        // fDoc = document;
    }

    @Override
    public void reconcile(final DirtyRegion dirtyRegion, final IRegion subRegion) {
        ErlLogger.error("reconcile called");
    }

    @Override
    public void reconcile(final IRegion partition) {
        ErlLogger.error("reconcile called");
    }

    @Override
    public void initialReconcile() {
        module = editor != null ? editor.getModule() : null;
        scanner = editor != null ? editor.getScanner() : null;
        if (module != null) {
            module.initialReconcile();
        }
        reconcile(true);
        // notify(new OtpErlangAtom("initialReconcile"));
    }

    private void reconcile(final boolean force) {
        if (documentProvider == null) {
            return;
        }
        final ISourceFile workingCopy = documentProvider.getWorkingCopy(editor
                .getEditorInput());
        if (workingCopy != null) {
            SafeRunner.run(new ISafeRunnable() {
                @Override
                public void run() throws CoreException {
                    workingCopy.reconcile(force, mon);
                }

                @Override
                public void handleException(final Throwable exception) {
                }
            });
        }
    }

    @Override
    public void setProgressMonitor(final IProgressMonitor monitor) {
        mon = monitor;
    }

    @Override
    public void uninstall() {
        if (module != null) {
            module.finalReconcile();
        }
    }

    @Override
    public void chunkReconciled() {
        if (module != null) {
            module.postReconcile(mon);
        }
    }

    @Override
    public void reconcile(final ErlDirtyRegion r) {
        if (module != null) {
            module.reconcileText(r.getOffset(), r.getLength(), r.getText(), mon);
        } else if (scanner != null) {
            scanner.replaceText(r.getOffset(), r.getLength(), r.getText());
        }
        reconcile(false);
    }

    public IErlModule getModule() {
        return module;
    }

}
