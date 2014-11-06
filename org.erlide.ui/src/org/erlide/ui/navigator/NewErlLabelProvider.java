package org.erlide.ui.navigator;

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.resource.LocalResourceManager;
import org.eclipse.jface.resource.ResourceManager;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.engine.new_model.IErlElement;

public class NewErlLabelProvider extends LabelProvider {
    private final ResourceManager resourceManager = new LocalResourceManager(
            JFaceResources.getResources());

    @Override
    public String getText(final Object element) {
        if (element instanceof IErlElement) {
            return ((IErlElement) element).getName();
        }
        return super.getText(element);
    }

    @Override
    public Image getImage(final Object element) {
        // if (element instanceof IFooDef) {
        // return Activator.getImage(Activator.IMG_OBJ_DEF);
        // }
        // if (element instanceof IFooVar) {
        // return Activator.getImage(Activator.IMG_OBJ_VAR);
        // }
        // IResource resource = null;
        // if (element instanceof IFooProject || element instanceof IFooFile) {
        // resource = ((IFooElement) element).getResource();
        // }
        // if (resource != null) {
        // final IWorkbenchAdapter adapter = (IWorkbenchAdapter) resource
        // .getAdapter(IWorkbenchAdapter.class);
        // if (adapter != null) {
        // return (Image)
        // resourceManager.get(adapter.getImageDescriptor(resource));
        // }
        // }
        return super.getImage(element);
    }

    @Override
    public void dispose() {
        resourceManager.dispose();
        super.dispose();
    }
}
