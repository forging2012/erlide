package org.erlide.ui.navigator.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.actions.OpenWithMenu;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionConstants;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonMenuConstants;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.ui.actions.ErlangSearchActionGroup;

public class ErlangFileActionProvider extends CommonActionProvider {

    private OpenErlangAction openAction;
    private ErlangSearchActionGroup searchActionGroup;

    public ErlangFileActionProvider() {
    }

    @Override
    public void init(final ICommonActionExtensionSite aSite) {
        super.init(aSite);
        final ICommonViewerSite viewSite = aSite.getViewSite();
        if (viewSite instanceof ICommonViewerWorkbenchSite) {
            final ICommonViewerWorkbenchSite workbenchSite = (ICommonViewerWorkbenchSite) viewSite;
            final IWorkbenchPartSite site = workbenchSite.getSite();
            openAction = new OpenErlangAction(aSite, workbenchSite.getSelectionProvider());
            searchActionGroup = new ErlangSearchActionGroup(site);
            final IContextService service = (IContextService) site
                    .getService(IContextService.class);
            service.activateContext("org.erlide.ui.erlangOutlineAndNavigatorScope");
        }
    }

    @Override
    public void fillActionBars(final IActionBars actionBars) {
        /* Set up the property open action when enabled. */
        if (openAction.isEnabled()) {
            actionBars.setGlobalActionHandler(ICommonActionConstants.OPEN, openAction);
        }
    }

    @Override
    public void fillContextMenu(final IMenuManager menu) {
        if (openAction.isEnabled()) {
            menu.appendToGroup(ICommonMenuConstants.GROUP_OPEN, openAction);
        }
        searchActionGroup.fillContextMenu(menu);
        addOpenWithMenu(menu);
    }

    private void addOpenWithMenu(final IMenuManager menu) {
        final IStructuredSelection selection = (IStructuredSelection) getContext()
                .getSelection();
        if (selection == null || selection.size() != 1) {
            return;
        }

        final Object element = selection.getFirstElement();

        IFile file = null;
        if (element instanceof IErlSource) {
            file = ((IErlSource) element).getFile();
        } else if (element instanceof IFile) {
            file = (IFile) element;
        }
        if (file != null) {
            final IMenuManager submenu = new MenuManager("Open Wit&h");
            submenu.add(new OpenWithMenu(getPage(), file));

            menu.appendToGroup(ICommonMenuConstants.GROUP_OPEN_WITH, submenu);
        }
    }

    private IWorkbenchPage getPage() {
        final ICommonActionExtensionSite actionSite = getActionSite();
        final ICommonViewerWorkbenchSite site = (ICommonViewerWorkbenchSite) actionSite
                .getViewSite();
        return site.getPage();
    }
}
