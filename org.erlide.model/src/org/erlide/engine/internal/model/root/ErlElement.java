/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.engine.internal.model.root;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.internal.util.ModelConfig;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.ErlModelStatus;
import org.erlide.engine.model.ErlModelStatusConstants;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementVisitor;
import org.erlide.util.ErlLogger;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;

/**
 * Root of Erlang element handle hierarchy.
 *
 * @see IErlElement
 */
public abstract class ErlElement extends PlatformObject implements IErlElement, Cloneable {

    /**
     * This element's parent, or <code>null</code> if this element does not have
     * a parent.
     */
    private final IErlElement fParent;

    private final List<IErlElement> fChildren = Lists.newArrayList();

    /**
     * This element's name, or an empty <code>String</code> if this element does
     * not have a name.
     */
    protected String fName;

    protected static final Object NO_INFO = new Object();

    /**
     * Constructs a handle for a Erlang element with the given parent element
     * and name.
     *
     * @param parent
     *            The parent of Erlang element
     * @param name
     *            The name of Erlang element
     *
     * @throws IllegalArgumentException
     *             if the type is not one of the valid Erlang element type
     *             constants
     *
     */
    protected ErlElement(final IErlElement parent, final String name) {
        fParent = parent;
        fName = name;
        assertThat(fName, is(not(nullValue())));
    }

    /**
     * This element is being closed. Do any necessary cleanup.
     *
     * @throws ErlModelException
     */
    protected void closing(final Object info) throws ErlModelException {
        for (final IErlElement e : getChildren()) {
            if (e instanceof ErlElement) {
                final ErlElement ee = (ErlElement) e;
                ee.closing(info);
            }
        }
    }

    /**
     * Returns true if this handle represents the same Erlang element as the
     * given handle. By default, two handles represent the same element if they
     * are identical or if they represent the same type of element, have equal
     * names, parents, and occurrence counts.
     *
     * <p>
     * If a subclass has other requirements for equality, this method must be
     * overridden.
     *
     * @see Object#equals
     */
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return false;
        }

        // Erlang model parent is null
        if (fParent == null) {
            return super.equals(o);
        }

        if (o instanceof ErlElement) { // WHY OH WHY?!?!?!? This was a tough
            // bug (jc)
            // assume instanceof check is done in subclass
            final ErlElement other = (ErlElement) o;
            return fName.equals(other.fName) && fParent.equals(other.fParent);
        }
        return false;
    }

    /**
     * @see IErlElement
     */
    @Override
    public IErlElement getAncestorOfKind(final ErlElementKind kind) {
        IErlElement element = this;
        while (true) {
            if (element.getKind() == kind) {
                return element;
            }
            final IErlElement parent = element.getParent();
            if (parent != null) {
                element = parent;
            } else {
                break;
            }
        }
        return null;
    }

    /**
     * @see IErlElement
     */
    @Override
    public String getName() {
        return fName;
    }

    /**
     * Return the first instance of IErlElement in the parent hierarchy of this
     * element.
     *
     * <p>
     * Subclasses that are not IErlElement's must override this method.
     */
    public IErlElement getErlElementParent() {
        return fParent;
    }

    /**
     * @see IErlElement
     */
    @Override
    public IErlElement getParent() {
        return fParent;
    }

    static class NoResourceSchedulingRule implements ISchedulingRule {

        public IPath fPath;

        public NoResourceSchedulingRule(final IPath path) {
            fPath = path;
        }

        @Override
        public boolean contains(final ISchedulingRule rule) {
            if (rule instanceof NoResourceSchedulingRule) {
                return fPath.isPrefixOf(((NoResourceSchedulingRule) rule).fPath);
            }
            return false;
        }

        @Override
        public boolean isConflicting(final ISchedulingRule rule) {
            if (rule instanceof NoResourceSchedulingRule) {
                final IPath otherPath = ((NoResourceSchedulingRule) rule).fPath;
                return fPath.isPrefixOf(otherPath) || otherPath.isPrefixOf(fPath);
            }
            return false;
        }
    }

    /**
     * @see IErlElement
     */
    @Override
    public boolean hasChildren() {
        synchronized (getModelLock()) {
            return !internalGetChildren().isEmpty();
        }
    }

    @Override
    public boolean hasChildrenOfKind(final ErlElementKind... kinds) {
        synchronized (getModelLock()) {
            for (final ErlElementKind kind : kinds) {
                for (final IErlElement child : internalGetChildren()) {
                    if (child.getKind() == kind) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Returns the hash code for this Erlang element. By default, the hash code
     * for an element is a combination of its name and parent's hash code.
     * Elements with other requirements must override this method.
     */
    @Override
    public int hashCode() {
        if (fParent == null) {
            return super.hashCode();
        }
        return Objects.hashCode(fName, fParent);
    }

    /**
     * @see IErlElement
     */
    @Override
    public boolean isReadOnly() {
        return false;
    }

    /**
     */
    public String readableName() {
        return getName();
    }

    protected String tabString(final int tab) {
        return " ";
        // final StringBuilder buffer = new StringBuilder();
        // for (int i = tab; i > 0; i--) {
        // buffer.append(" "); //$NON-NLS-1$
        // }
        // return buffer.toString();
    }

    /**
     * Debugging purposes
     */
    public String toDebugString() {
        final StringBuilder buffer = new StringBuilder();
        this.toStringInfo(0, buffer, NO_INFO);
        return buffer.toString();
    }

    @Override
    public String toString() {
        final StringBuilder buffer = new StringBuilder();
        toString(0, buffer);
        return buffer.toString();
    }

    /**
     * Debugging purposes
     */
    protected void toString(final int tab, final StringBuilder buffer) {
        final Object info = this.toStringInfo(tab, buffer);
        if (tab == 0) {
            // toStringAncestors(buffer);
        }
        toStringChildren(tab, buffer, info);
    }

    /**
     * Debugging purposes
     */
    @Override
    public String toStringWithAncestors() {
        final StringBuilder buffer = new StringBuilder();
        this.toStringInfo(0, buffer, NO_INFO);
        toStringAncestors(buffer);
        return buffer.toString();
    }

    /**
     * Debugging purposes
     */
    protected void toStringAncestors(final StringBuilder buffer) {
        final IErlElement parent = getParent();
        if (parent != null) {
            if (parent instanceof ErlElement) {
                final ErlElement parentElement = (ErlElement) parent;
                buffer.append("[> "); //$NON-NLS-1$
                parentElement.toStringInfo(0, buffer, NO_INFO);
                parentElement.toStringAncestors(buffer);
            }
            buffer.append("] "); //$NON-NLS-1$
        }
    }

    /**
     * Debugging purposes
     */
    protected void toStringChildren(final int tab, final StringBuilder buffer,
            final Object info) {
        if (!(info instanceof ErlElement)) {
            return;
        }
        if (getChildCount() > 0) {
            buffer.append("{");
            int i = 0;
            try {
                for (final IErlElement element : getChildren()) {
                    ((ErlElement) element).toString(tab + 1, buffer);
                    buffer.append(","); //$NON-NLS-1$
                    if (++i > 5) {
                        buffer.append("...");
                        break;
                    }
                }
            } catch (final ErlModelException e) {
            }
            buffer.deleteCharAt(buffer.length() - 1);
            buffer.append("}");
        }
    }

    /**
     * Debugging purposes
     */
    public Object toStringInfo(final int tab, final StringBuilder buffer) {
        this.toStringInfo(tab, buffer, this);
        return this;
    }

    /**
     * Debugging purposes
     */
    protected void toStringInfo(final int tab, final StringBuilder buffer,
            final Object info) {
        buffer.append(tabString(tab));
        buffer.append(getName());
        if (info == null) {
            buffer.append(" (not open)"); //$NON-NLS-1$
        }
    }

    /**
     * Is the structure of this element known
     *
     * @see IErlElement#isStructureKnown()
     */
    protected boolean structureKnown = false;

    @Override
    public void clearCaches() {
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    @Override
    public List<IErlElement> getChildren() throws ErlModelException {
        synchronized (getModelLock()) {
            return Collections
                    .unmodifiableList(Lists.newArrayList(internalGetChildren()));
        }
    }

    public List<IErlElement> internalGetChildren() {
        return fChildren;
    }

    @Override
    public int getChildCount() {
        synchronized (getModelLock()) {
            return internalGetChildren().size();
        }
    }

    /**
     * Returns a collection of (immediate) children of this node of the
     * specified type.
     *
     * @param type
     *            - one of the constants defined by IErlElement
     */
    @Override
    public List<IErlElement> getChildrenOfKind(final ErlElementKind... kinds)
            throws ErlModelException {
        final List<IErlElement> result = Lists.newArrayList();
        synchronized (getModelLock()) {
            for (final ErlElementKind kind : kinds) {
                for (final IErlElement element : internalGetChildren()) {
                    if (element.getKind() == kind) {
                        result.add(element);
                    }
                }
            }
        }
        return result;
    }

    @Override
    public IErlElement getChildNamed(final String name) {
        return getChildNamed(this, name);
    }

    @Override
    public IErlElement getChildWithResource(final IResource rsrc) {
        return getChildWithResource(this, rsrc);
    }

    /**
     * Returns <code>true</code> if this child is in my children collection
     */
    protected boolean includesChild(final IErlElement child) {
        synchronized (getModelLock()) {
            return internalGetChildren().contains(child);
        }
    }

    /**
     * @see IErlElement#isStructureKnown()
     */
    @Override
    public boolean isStructureKnown() {
        return structureKnown;
    }

    @Override
    public void removeChild(final IErlElement child) {
        synchronized (getModelLock()) {
            clearCaches();
            fChildren.remove(child);
        }
    }

    @Override
    public void addChild(final IErlElement child) {
        synchronized (getModelLock()) {
            clearCaches();
            fChildren.add(child);
        }
    }

    @Override
    public void setChildren(final Collection<? extends IErlElement> children) {
        synchronized (getModelLock()) {
            clearCaches();
            fChildren.clear();
            if (children != null) {
                fChildren.addAll(children);
            }
        }
    }

    public void setStructureKnown(final boolean newStructureKnown) {
        structureKnown = newStructureKnown;
    }

    @Override
    public void resourceChanged(final IResourceDelta delta) {
        setStructureKnown(false);
    }

    private static IErlElement getChildNamed(final ErlElement parent, final String name) {
        synchronized (parent.getModelLock()) {
            for (final IErlElement child : parent.internalGetChildren()) {
                if (child.getName().equals(name)) {
                    return child;
                }
            }
        }
        return null;
    }

    protected Object getModelLock() {
        return ErlangEngine.getInstance().getModel().getModelLock();
    }

    private static IErlElement getChildWithResource(final ErlElement parent,
            final IResource rsrc) {
        synchronized (parent.getModelLock()) {
            for (final IErlElement child : parent.internalGetChildren()) {
                if (rsrc.equals(child.getResource())) {
                    return child;
                }
            }
        }
        return null;
    }

    @Override
    public final void accept(final IErlElementVisitor visitor,
            final Set<AcceptFlags> flags, final ErlElementKind leafKind)
            throws ErlModelException {
        synchronized (getModelLock()) {
            internalAccept(visitor, flags, leafKind);
        }
    }

    private final void internalAccept(final IErlElementVisitor visitor,
            final Set<AcceptFlags> flags, final ErlElementKind leafKind)
            throws ErlModelException {
        if (getKind() == leafKind) {
            visitor.visit(this);
        } else {
            boolean visitChildren = true;
            if (!flags.contains(AcceptFlags.LEAFS_ONLY)
                    && !flags.contains(AcceptFlags.CHILDREN_FIRST)) {
                visitChildren = visitor.visit(this);
            }
            if (visitChildren) {
                for (final IErlElement child : internalGetChildren()) {
                    child.accept(visitor, flags, leafKind);
                }
            }
            if (!flags.contains(AcceptFlags.LEAFS_ONLY)
                    && flags.contains(AcceptFlags.CHILDREN_FIRST)) {
                visitor.visit(this);
            }
        }
    }

    /**
     * Return my corresponding resource. Overridden in IErlModule, IErlFolder
     * and IErlProject
     */
    @Override
    public IResource getCorrespondingResource() {
        return null;
    }

    @Override
    public IResource getResource() {
        if (fParent != null) {
            final IErlElement parentElement = fParent;
            return parentElement.getResource();
        }
        return null;
    }

    @Override
    public String getFilePath() {
        return null;
    }

    @Override
    public void dispose() {
        // if (!LOCAL_CHILDREN) {
        // getModel().setChildrenOf(this, null);
        // }
    }

    protected ErlModelCache getModelCache() {
        return ErlModelCache.getDefault();
    }

    // /////////////////////////////////////////////////////

    protected IResource findResult;

    /**
     * Builds this element's structure and properties in the given info object,
     * based on this element's current contents (reuse buffer contents if this
     * element has an open buffer, or resource contents if this element does not
     * have an open buffer). Children are placed in the given newElements table
     * (note, this element has already been placed in the newElements table).
     * Returns true if successful, or false if an error is encountered while
     * determining the structure of this element.
     *
     * @param dirtyRegion
     */
    public abstract boolean buildStructure(IProgressMonitor pm) throws ErlModelException;

    @Override
    public boolean exists() {
        final IResource resource = getResource();
        if (resource != null) {
            return resource.exists();
        }
        return true;
    }

    @Override
    public synchronized void open(final IProgressMonitor monitor)
            throws ErlModelException {
        if (ModelConfig.verbose) {
            ErlLogger.debug("open " + isStructureKnown() + " > " + this);
        }
        // open the parent if necessary
        openParent(monitor);
        if (monitor != null && monitor.isCanceled()) {
            return;
        }

        // build the structure of the ErlElement (this will open the buffer if
        // needed)
        if (!isStructureKnown()) {
            final boolean knownStructure = buildStructure(monitor);
            setStructureKnown(knownStructure);
        }
    }

    class ErlangResourceVisitor implements IResourceVisitor {

        private final String aname;

        public ErlangResourceVisitor(final String name) {
            aname = name;
        }

        @Override
        public boolean visit(final IResource resource) {
            if (resource.getType() == IResource.FILE && resource.getName().equals(aname)) {
                findResult = resource;
                return false;
            }
            // return true to continue visiting children.
            return true;
        }
    }

    /**
     * Returns true if this element may have an associated source buffer,
     * otherwise false. Subclasses must override as required.
     */
    protected boolean hasBuffer() {
        return false;
    }

    @Override
    public boolean hasUnsavedChanges() throws ErlModelException {

        if (isReadOnly() || !isOpen()) {
            return false;
        }
        // final IBuffer buf = this.getBuffer();
        // if (buf != null && buf.hasUnsavedChanges()) {
        // return true;
        // }
        // for packages and projects must check open buffers
        // to see if they have an child with unsaved changes
        final ErlElementKind elementType = getKind();
        if (elementType == ErlElementKind.PROJECT || elementType == ErlElementKind.MODEL) {
            // final Enumeration openBuffers =
            // getBufferManager().getOpenBuffers();
            // while (openBuffers.hasMoreElements()) {
            // final IBuffer buffer = (IBuffer) openBuffers.nextElement();
            // if (buffer.hasUnsavedChanges()) {
            // final IErlElement owner = (IErlElement) buffer.getOwner();
            // if (isAncestorOf(owner)) {
            // return true;
            // }
            // }
            // }
        }

        return false;
    }

    @Override
    public boolean isConsistent() {
        return true;
    }

    @Override
    public boolean isOpen() {
        return true;
    }

    protected boolean isSourceElement() {
        return false;
    }

    @Override
    public void makeConsistent(final IProgressMonitor monitor) throws ErlModelException {
        if (isConsistent()) {
            return;
        }
    }

    protected void openParent(final IProgressMonitor pm) throws ErlModelException {

        final ErlElement ErlElementParent = (ErlElement) getErlElementParent();
        if (ErlElementParent != null && !ErlElementParent.isOpen()) {
            ErlElementParent.open(pm);
        }
    }

    protected boolean parentExists() {
        final IErlElement parent = getParent();
        if (parent == null) {
            return true;
        }
        {
            final IErlElement element = parent;
            return element.exists();
        }
    }

    protected boolean resourceExists() {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        if (workspace == null) {
            return false;
        }
        return ErlangEngine
                .getInstance()
                .getModelUtilService()
                .getTarget(workspace.getRoot(),
                        getResource().getFullPath().makeRelative(), true) != null;
    }

    @Override
    public void save(final IProgressMonitor pm, final boolean force)
            throws ErlModelException {
        if (isReadOnly()) {
            throw new ErlModelException(new ErlModelStatus(
                    ErlModelStatusConstants.READ_ONLY, this));
        }
        // final IBuffer buf = getBuffer();
        // if (buf != null) { // some ErlElements (like a ErlProject) don't have
        // a
        // // buffer
        // buf.save(pm, force);
        // makeConsistent(pm); // update the element info of this
        // // element
        // }
    }

    @Override
    public void close() throws ErlModelException {
        for (final IErlElement child : getChildren()) {
            if (child != null) {
                final IErlElement ErlElement = child;
                if (ErlElement.isOpen()) {
                    ErlElement.close();
                }
            }
        }
        internalGetChildren().clear();
        setStructureKnown(false);
    }

}
