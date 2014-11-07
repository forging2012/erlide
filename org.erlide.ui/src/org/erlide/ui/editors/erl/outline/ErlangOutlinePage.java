/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.ui.editors.erl.outline;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.handly.model.IElementChangeEvent;
import org.eclipse.handly.model.IElementChangeListener;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.IHandleDelta;
import org.eclipse.handly.model.ISourceElement;
import org.eclipse.handly.model.ISourceFile;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.commands.ActionHandler;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.OpenAndLinkWithEditorHelper;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.eclipse.ui.part.IPageSite;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.erlide.core.ErlangCore;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlModelChangeListener;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.new_model.ErlModelCore;
import org.erlide.ui.ErlideImage;
import org.erlide.ui.actions.ActionMessages;
import org.erlide.ui.actions.CompositeActionGroup;
import org.erlide.ui.actions.ErlangSearchActionGroup;
import org.erlide.ui.actions.SortAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;
import org.erlide.ui.editors.erl.outline.filters.FilterDescriptor;
import org.erlide.ui.editors.erl.outline.filters.OutlineFilterUtils;
import org.erlide.ui.editors.erl.outline.filters.PatternFilter;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.navigator.ErlElementSorter;
import org.erlide.ui.navigator.ErlangFileContentProvider;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.prefs.plugin.ErlEditorMessages;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.SystemConfiguration.Features;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

/**
 *
 * @author Vlad Dumitrescu
 *
 */

public class ErlangOutlinePage extends ContentOutlinePage implements
        IErlModelChangeListener, ISortableContentOutlinePage, IElementChangeListener {

    IErlModule fModule;
    private ErlangEditor editor;
    private CompositeActionGroup fActionGroups;
    private TreeViewer fOutlineViewer;
    private SortAction fSortAction;
    private LinkingHelper fLinkHelper;
    private ToggleLinkingAction fToggleLinkingAction;
    private final PatternFilter fPatternFilter = new PatternFilter();
    private IPartListener fPartListener;

    @Override
    public Control getControl() {
        return fOutlineViewer.getControl();
    }

    @Override
    public ISelection getSelection() {
        return fOutlineViewer.getSelection();
    }

    @Override
    public TreeViewer getTreeViewer() {
        return fOutlineViewer;
    }

    @Override
    public void setFocus() {
        getControl().setFocus();
    }

    @Override
    public void setSelection(final ISelection selection) {
        fOutlineViewer.setSelection(selection);
    }

    /**
     *
     * @param documentProvider
     *
     * @param editor
     *
     */
    public ErlangOutlinePage(final ErlangEditor editor) {
        this.editor = editor;
        ErlangEngine.getInstance().getModel().addModelChangeListener(this);
        ErlModelCore.getErlModel().addElementChangeListener(this);
        editor.addPropertyListener(editorInputListener);
    }

    private final IPropertyListener editorInputListener = new IPropertyListener() {
        @Override
        public void propertyChanged(final Object source, final int propId) {
            if (propId == IEditorPart.PROP_INPUT) {
                getTreeViewer().setInput(computeInput());
            }
        }
    };

    /**
     *
     * @param editorInput
     *
     */
    public void setInput(final IEditorInput editorInput) {
        fModule = null;
        try {
            fModule = ErlModelUtils.getModule(editorInput);
            if (fModule != null) {
                fModule.open(null);
            }
            // addFilters();
        } catch (final CoreException e) {
        }
    }

    private void addFilters() {
        final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
        final Collection<FilterDescriptor> filterDescriptors = FilterDescriptor
                .getFilterDescriptors();
        final List<FilterDescriptor> descs = Lists.newArrayList();
        for (final FilterDescriptor filterDescriptor : filterDescriptors) {
            final String filterId = filterDescriptor.getId();
            final boolean value = prefsNode.getBoolean(filterId, false);
            if (value) {
                descs.add(filterDescriptor);
            }
        }
        OutlineFilterUtils.setFilters(descs, this);
        final List<String> userDefinedPatterns = Lists.newArrayList();
        final Set<String> enabledFilterIDs = Sets.newHashSet();
        final List<String> emptyList = Lists.newArrayList();
        final Set<String> emptySet = Sets.newHashSet();
        final boolean userFiltersEnabled = OutlineFilterUtils.loadViewDefaults(
                userDefinedPatterns, enabledFilterIDs);
        if (!userFiltersEnabled) {
            userDefinedPatterns.clear();
        }
        OutlineFilterUtils.updateViewerFilters(getTreeViewer(), emptyList, emptySet,
                userDefinedPatterns, enabledFilterIDs, getPatternFilter());
    }

    public void refresh() {
        if (getTreeViewer() != null) {
            final Control c = getTreeViewer().getControl();
            if (c.isDisposed()) {
                return;
            }
            final Display d = c.getDisplay();
            d.asyncExec(new Runnable() {

                @Override
                public void run() {
                    c.setRedraw(false);
                    if (getTreeViewer().getControl() != null
                            && !getTreeViewer().getControl().isDisposed()) {
                        final TreePath[] treePaths = getTreeViewer()
                                .getExpandedTreePaths();
                        getTreeViewer().refresh();
                        getTreeViewer().setExpandedTreePaths(treePaths);
                    }
                    c.setRedraw(true);
                }
            });
        }
    }

    @Override
    public void createControl(final Composite parent) {
        final Tree tree = new Tree(parent, SWT.MULTI);
        fOutlineViewer = new TreeViewer(tree);
        fOutlineViewer.setAutoExpandLevel(0);
        fOutlineViewer.setUseHashlookup(true);
        if (SystemConfiguration.hasFeatureEnabled(Features.NEW_MODEL)) {
            // FIXME content provider!
            fOutlineViewer.setContentProvider(new ErlangFileContentProvider());
            // fOutlineViewer.setLabelProvider(editor.createOutlineLabelProvider());
            fOutlineViewer.setInput(computeInput());
        } else {
            fOutlineViewer.setContentProvider(editor.createOutlineContentProvider());
            fOutlineViewer.setLabelProvider(editor.createOutlineLabelProvider());
            fOutlineViewer.setInput(fModule);
        }

        fOutlineViewer.addPostSelectionChangedListener(this);
        final IPageSite site = getSite();
        fLinkHelper = new LinkingHelper();

        final IContextService service = (IContextService) site
                .getService(IContextService.class);
        service.activateContext("org.erlide.ui.erlangOutlineAndNavigatorScope");

        final MenuManager manager = new MenuManager();
        manager.setRemoveAllWhenShown(true);
        manager.addMenuListener(new IMenuListener() {
            @Override
            public void menuAboutToShow(final IMenuManager m) {
                // recursive loop?
                // menuAboutToShow(m);
                contextMenuAboutToShow(m);
            }
        });
        final Menu menu = manager.createContextMenu(tree);
        tree.setMenu(menu);

        site.registerContextMenu(ErlangCore.PLUGIN_ID + ".outline", manager,
                fOutlineViewer);
        fActionGroups = new CompositeActionGroup(
                new ActionGroup[] { new ErlangSearchActionGroup(this) });
        // register global actions
        final IActionBars actionBars = site.getActionBars();
        actionBars.setGlobalActionHandler(ITextEditorActionConstants.UNDO,
                editor.getAction(ITextEditorActionConstants.UNDO));
        actionBars.setGlobalActionHandler(ITextEditorActionConstants.REDO,
                editor.getAction(ITextEditorActionConstants.REDO));
        fActionGroups.fillActionBars(actionBars);
        registerToolbarActions(actionBars);
        final IHandlerService handlerService = (IHandlerService) site
                .getService(IHandlerService.class);
        handlerService.activateHandler(
                IWorkbenchCommandConstants.NAVIGATE_TOGGLE_LINK_WITH_EDITOR,
                new ActionHandler(fToggleLinkingAction));
        fPartListener = new IPartListener() {

            @Override
            public void partOpened(final IWorkbenchPart part) {
                addFilters(); // JC borde filter-metoden ovan r�cka?
            }

            @Override
            public void partDeactivated(final IWorkbenchPart part) {
            }

            @Override
            public void partClosed(final IWorkbenchPart part) {
            }

            @Override
            public void partBroughtToTop(final IWorkbenchPart part) {
            }

            @Override
            public void partActivated(final IWorkbenchPart part) {
                addFilters();
            }
        };
        getSite().getPage().addPartListener(fPartListener);
    }

    protected void contextMenuAboutToShow(final IMenuManager menu) {
        ErlideUIPlugin.createStandardGroups(menu);
        final IStructuredSelection selection = (IStructuredSelection) getSelection();
        fActionGroups.setContext(new ActionContext(selection));
        fActionGroups.fillContextMenu(menu);
    }

    public static class NoModuleElement extends WorkbenchAdapter implements IAdaptable {

        /*
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return ErlEditorMessages.ErlangOutlinePage_error_noelement;
        }

        /*
         * 
         * @see org.eclipse.core.runtime.IAdaptable#getAdapter(Class)
         */
        @Override
        public Object getAdapter(final Class clas) {
            if (clas == IWorkbenchAdapter.class) {
                return this;
            }
            return null;
        }
    }

    public void select(final ISourceReference reference) {
        if (getTreeViewer() != null) {
            ISelection s = getTreeViewer().getSelection();
            if (s instanceof IStructuredSelection) {
                final IStructuredSelection ss = (IStructuredSelection) s;
                final List<?> elements = ss.toList();
                if (!elements.contains(reference)) {
                    s = reference == null ? StructuredSelection.EMPTY
                            : new StructuredSelection(reference);
                    getTreeViewer().setSelection(s, true);
                }
            }
        }
    }

    @Override
    public void dispose() {
        getSite().getPage().removePartListener(fPartListener);
        ErlangEngine.getInstance().getModel().removeModelChangeListener(this);
        ErlModelCore.getErlModel().removeElementChangeListener(this);
        fLinkHelper.dispose();
        editor.removePropertyListener(editorInputListener);
        editor.outlinePageClosed();
        editor = null;

        super.dispose();
    }

    @Override
    public void elementChanged(final IErlElement element) {
        if (fModule == element) {
            refresh();
        }
    }

    /**
     * @param actionBars
     */
    private void registerToolbarActions(final IActionBars actionBars) {
        final IToolBarManager toolBarManager = actionBars.getToolBarManager();
        fSortAction = new SortAction(getTreeViewer(), "Sort", new ErlElementSorter(
                ErlElementSorter.SORT_ON_NAME), new ErlElementSorter(
                ErlElementSorter.SORT_ON_EXPORT), null, false, ErlideUIPlugin
                .getDefault().getPreferenceStore());
        toolBarManager.add(fSortAction);

        final IMenuManager viewMenuManager = actionBars.getMenuManager();
        fToggleLinkingAction = new ToggleLinkingAction();
        fToggleLinkingAction
                .setActionDefinitionId(IWorkbenchCommandConstants.NAVIGATE_TOGGLE_LINK_WITH_EDITOR);
        viewMenuManager.add(fToggleLinkingAction);
    }

    @Override
    public void sort(final boolean sorting) {
        ErlLogger.debug("NYI: sorting " + sorting);
    }

    public static IEclipsePreferences getPrefsNode() {
        final String qualifier = ErlideUIPlugin.PLUGIN_ID;
        final IScopeContext context = InstanceScope.INSTANCE;
        final IEclipsePreferences eclipsePreferences = context.getNode(qualifier);
        return eclipsePreferences;
    }

    /**
     * This action toggles whether this Erlang Outline page links its selection
     * to the active editor.
     *
     * @since 3.0
     */
    public class ToggleLinkingAction extends Action {

        /**
         * Constructs a new action.
         */
        public ToggleLinkingAction() {
            super(ActionMessages.ToggleLinkingAction_label);
            setDescription(ActionMessages.ToggleLinkingAction_description);
            setToolTipText(ActionMessages.ToggleLinkingAction_tooltip);
            ErlideImage.setLocalImageDescriptors(this, "synced.gif");
            PlatformUI.getWorkbench().getHelpSystem()
                    .setHelp(this, IErlangHelpContextIds.LINK_EDITOR_ACTION);
            final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
            final boolean isLinkingEnabled = prefsNode.getBoolean(
                    PreferenceConstants.ERLANG_OUTLINE_LINK_WITH_EDITOR, true);
            setChecked(isLinkingEnabled);
            fLinkHelper.setLinkWithEditor(isLinkingEnabled);
        }

        /**
         * Runs the action.
         */
        @Override
        public void run() {
            final IEclipsePreferences prefsNode = ErlangOutlinePage.getPrefsNode();
            final boolean isChecked = isChecked();
            prefsNode.putBoolean(PreferenceConstants.ERLANG_OUTLINE_LINK_WITH_EDITOR,
                    isChecked);
            if (isChecked && editor != null) {
                getTreeViewer().refresh();
            }
            fLinkHelper.setLinkWithEditor(isChecked);
        }

    }

    public PatternFilter getPatternFilter() {
        return fPatternFilter;
    }

    public boolean isLinkedWithEditor() {
        return fLinkHelper.isLinkedWithEditor();
    }

    @Override
    public void elementChanged(final IElementChangeEvent event) {
        if (!(getTreeViewer().getInput() instanceof IHandle)) {
            return;
        }
        if (affects(event.getDelta(), (IHandle) getTreeViewer().getInput())) {
            final Control control = getTreeViewer().getControl();
            control.getDisplay().asyncExec(new Runnable() {
                @Override
                public void run() {
                    if (!control.isDisposed()) {
                        refresh();
                    }
                }
            });
        }
    }

    private boolean affects(final IHandleDelta delta, final IHandle element) {
        if (delta.getElement().equals(element)) {
            return true;
        }
        final IHandleDelta[] children = delta.getAffectedChildren();
        for (final IHandleDelta child : children) {
            if (affects(child, element)) {
                return true;
            }
        }
        return false;
    }

    private Object computeInput() {
        final IEditorInput editorInput = editor.getEditorInput();
        if (editorInput instanceof IFileEditorInput) {
            final IFile file = ((IFileEditorInput) editorInput).getFile();
            return ErlModelCore.create(file);
        }
        return null;
    }

    private class LinkingHelper extends OpenAndLinkWithEditorHelper {
        private final ISelectionChangedListener editorListener = new ISelectionChangedListener() {
            @Override
            public void selectionChanged(final SelectionChangedEvent event) {
                if (!getTreeViewer().getControl().isFocusControl()) {
                    linkToOutline(event.getSelection());
                }
            }
        };
        private boolean linking;

        public LinkingHelper() {
            super(getTreeViewer());
            setLinkWithEditor(true);
            final ISelectionProvider selectionProvider = editor.getSite()
                    .getSelectionProvider();
            if (selectionProvider instanceof IPostSelectionProvider) {
                ((IPostSelectionProvider) selectionProvider)
                        .addPostSelectionChangedListener(editorListener);
            } else {
                selectionProvider.addSelectionChangedListener(editorListener);
            }
        }

        public boolean isLinkedWithEditor() {
            return linking;
        }

        @Override
        public void dispose() {
            final ISelectionProvider selectionProvider = editor.getSite()
                    .getSelectionProvider();
            if (selectionProvider instanceof IPostSelectionProvider) {
                ((IPostSelectionProvider) selectionProvider)
                        .removePostSelectionChangedListener(editorListener);
            } else {
                selectionProvider.removeSelectionChangedListener(editorListener);
            }
            super.dispose();
        }

        @Override
        public void setLinkWithEditor(final boolean enabled) {
            super.setLinkWithEditor(enabled);
            linking = enabled;
            if (enabled) {
                linkToOutline(editor.getSite().getSelectionProvider().getSelection());
            }
        }

        @Override
        protected void activate(final ISelection selection) {
            linkToEditor(selection);
        }

        @Override
        protected void open(final ISelection selection, final boolean activate) {
            linkToEditor(selection);
        }

        @Override
        protected void linkToEditor(final ISelection selection) {
            if (selection == null || selection.isEmpty()) {
                return;
            }
            final Object element = ((IStructuredSelection) selection).getFirstElement();
            if (!(element instanceof ISourceElement)) {
                return;
            }
            SourceElementUtil.revealInTextEditor(editor, (ISourceElement) element);
        }

        @SuppressWarnings("unchecked")
        protected void linkToOutline(final ISelection selection) {
            if (selection == null || selection.isEmpty()) {
                return;
            }
            IStructuredSelection linkedSelection = null;
            if (selection instanceof ITextSelection) {
                linkedSelection = getLinkedSelection((ITextSelection) selection);
            }
            if (linkedSelection != null) {
                final IStructuredSelection currentSelection = (IStructuredSelection) getTreeViewer()
                        .getSelection();
                if (currentSelection == null
                        || !currentSelection.toList().containsAll(
                                linkedSelection.toList())) {
                    getTreeViewer().setSelection(linkedSelection, true);
                }
            }
        }

        private IStructuredSelection getLinkedSelection(final ITextSelection selection) {
            final Object input = getTreeViewer().getInput();
            if (!(input instanceof ISourceElement)) {
                return null;
            }
            final ISourceFile sourceFile = ((ISourceElement) input).getSourceFile();
            final ISourceElement element = SourceElementUtil.getSourceElement(sourceFile,
                    selection.getOffset());
            if (element == null) {
                return null;
            }
            return new StructuredSelection(element);
        }
    }
}
