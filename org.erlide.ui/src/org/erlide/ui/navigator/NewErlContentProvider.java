package org.erlide.ui.navigator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.engine.new_model.IErlElement;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.SystemConfiguration.Features;

public class NewErlContentProvider implements ITreeContentProvider {
    protected static final Object[] NO_CHILDREN = new Object[0];

    @Override
    public Object[] getElements(final Object inputElement) {
        return getChildren(inputElement);
    }

    @Override
    public Object[] getChildren(final Object parentElement) {
        if (!SystemConfiguration.hasFeatureEnabled(Features.NEW_MODEL)) {
            return NO_CHILDREN;
        }
        if (parentElement instanceof IProject || parentElement instanceof IFile) {
            final IResource res = (IResource) parentElement;
            final IErlElement elem = ErlModelCore.create(res);
            if (elem == null || !elem.exists()) {
                return NO_CHILDREN;
            }
            try {
                return elem.getChildren();
            } catch (final CoreException e) {
            }
        }
        if (parentElement instanceof IErlElement) {
            try {
                return ((IErlElement) parentElement).getChildren();
            } catch (final CoreException e) {
            }
        }
        return NO_CHILDREN;
    }

    @Override
    public Object getParent(final Object element) {
        if (element instanceof IErlElement) {
            return ((IErlElement) element).getParent();
        }
        return null;
    }

    @Override
    public boolean hasChildren(final Object element) {
        return getChildren(element).length > 0;
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
    }

    @Override
    public void dispose() {
    }
}
