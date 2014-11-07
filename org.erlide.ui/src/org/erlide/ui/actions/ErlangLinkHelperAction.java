package org.erlide.ui.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.ILinkHelper;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.navigator.ErlangNavigator;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.SystemConfiguration.Features;

public class ErlangLinkHelperAction implements ILinkHelper {

    @Override
    public void activateEditor(final IWorkbenchPage page,
            final IStructuredSelection selection) {
        final Object element = selection.getFirstElement();
        final IEditorPart part = EditorUtility.isOpenInEditor(element);
        if (part != null) {
            page.bringToTop(part);
            if (element instanceof IErlElement) {
                EditorUtility.revealInEditor(part, (IErlElement) element);
            }
        }
    }

    @Override
    public IStructuredSelection findSelection(final IEditorInput input) {
        if (SystemConfiguration.hasFeatureEnabled(Features.NEW_MODEL)) {
            if (input instanceof IFileEditorInput) {
                final IFile file = ((IFileEditorInput) input).getFile();
                final IErlSource fooFile = ErlModelCore.create(file);
                if (fooFile != null) {
                    final IViewPart navigatorView = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getActivePage()
                            .findView(ErlangNavigator.ID);
                    if (navigatorView != null) {
                        final IStructuredSelection currentSelection = (IStructuredSelection) navigatorView
                                .getSite().getSelectionProvider().getSelection();
                        if (currentSelection != null && currentSelection.size() == 1) {
                            final Object element = currentSelection.getFirstElement();
                            if (element instanceof org.erlide.engine.new_model.IErlElement) {
                                if (fooFile
                                        .equals(((org.erlide.engine.new_model.IErlElement) element)
                                                .getAncestor(IErlSource.class))) {
                                    return currentSelection;
                                }
                            }
                        }
                    }
                    return new StructuredSelection(fooFile);
                }
                return new StructuredSelection(file);
            }
        } else {
            try {
                final IErlModule module = ErlModelUtils.getModule(input);
                if (module != null) {
                    final IResource resource = module.getCorrespondingResource();
                    if (resource != null) {
                        return new StructuredSelection(resource);
                    }
                    return new StructuredSelection(module);
                }
            } catch (final CoreException e) {
            }
        }
        return StructuredSelection.EMPTY;
    }

}
