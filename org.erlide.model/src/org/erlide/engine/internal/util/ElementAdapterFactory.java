package org.erlide.engine.internal.util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.new_model.ErlModelCore;

public class ElementAdapterFactory implements IAdapterFactory {

    @SuppressWarnings("rawtypes")
    private static final Class[] ADAPTER_LIST = new Class[] { IErlElement.class,
            org.erlide.engine.new_model.IErlElement.class, IResource.class };

    @Override
    public Object getAdapter(final Object adaptableObject, final Class adapterType) {
        if (adapterType == IErlElement.class && adaptableObject instanceof IResource) {
            return ErlangEngine.getInstance().getModel()
                    .findElement((IResource) adaptableObject);
        }
        if (adapterType == org.erlide.engine.new_model.IErlElement.class
                && adaptableObject instanceof IResource) {
            return ErlModelCore.create((IResource) adaptableObject);
        }
        if (adapterType == IResource.class
                && adaptableObject instanceof org.erlide.engine.new_model.IErlElement) {
            final org.erlide.engine.new_model.IErlElement element = (org.erlide.engine.new_model.IErlElement) adaptableObject;
            return element.getResource();
        }
        return null;
    }

    @Override
    public Class[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
