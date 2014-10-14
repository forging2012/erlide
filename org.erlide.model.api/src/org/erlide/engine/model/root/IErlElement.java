/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.model.root;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.model.ErlModelException;
import org.erlide.util.IDisposable;

/**
 * Common protocol for all elements provided by the Erlang model. Erlang model
 * elements are exposed to clients as handles to the actual underlying element.
 * The Erlang model may hand out any number of handles for each element. Handles
 * that refer to the same element are guaranteed to be equal, but not
 * necessarily identical.
 * <p>
 * Methods annotated as "handle-only" do not require underlying elements to
 * exist. Methods that require underlying elements to exist throw a
 * <code>ErlModelException</code> when an underlying element is missing.
 * <code>ErlModelException.isDoesNotExist</code> can be used to recognize this
 * common special case.
 * </p>
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 * @author jakob
 */
public interface IErlElement extends IAdaptable, IDisposable {

    /**
     * Returns whether this Erlang element exists in the model.
     * <p>
     * Erlang elements are handle objects that may or may not be backed by an
     * actual element. Erlang elements that are backed by an actual element are
     * said to "exist", and this method returns <code>true</code>. It is always
     * the case that if the element exists, then its parent also exists
     * (provided it has one) and includes the element as one of its children. It
     * is therefore possible to navigate to any existing Erlang element from the
     * root of the Erlang model along a chain of existing Erlang elements.
     * </p>
     *
     * @return <code>true</code> if this element exists in the Erlang model, and
     *         <code>false</code> if this element does not exist
     */
    boolean exists();

    /**
     * Returns the first ancestor of this Erlang element that has the given
     * type. Returns <code>null</code> if no such an ancestor can be found. This
     * is a handle-only method.
     *
     * @param kind
     *            the given kind
     * @return the first ancestor of this Erlang element that has the given
     *         kind, null if no such an ancestor can be found
     */
    IErlElement getAncestorOfKind(ErlElementKind kind);

    /**
     * Returns the resource that corresponds directly to this element, or
     * <code>null</code> if there is no resource that corresponds to this
     * element.
     * <p>
     * For example, the corresponding resource for an <code>IErlModule</code> is
     * its underlying <code>IFile</code>. There are no corresponding resources
     * for <code>IAttributes</code>,<code>IFunctions</code>, etc.
     * <p>
     * This differs from getResource, which returns the parents resource, if the
     * element doesn't correspond directly to a resource.
     *
     * @return the corresponding resource, or <code>null</code> if none
     */
    IResource getCorrespondingResource();

    /**
     * Returns the name of this element. This is a handle-only method.
     *
     * @return the element name
     */
    String getName();

    /**
     * Returns this element's kind encoded as an enum. This is a handle-only
     * method.
     *
     * @return the kind of element
     * @see IErlElement
     */
    ErlElementKind getKind();

    /**
     * Returns the element directly containing this element, or
     * <code>null</code> if this element has no parent. This is a handle-only
     * method.
     *
     * @return the parent element, or <code>null</code> if this element has no
     *         parent
     */
    IErlElement getParent();

    /**
     * Returns the innermost resource enclosing this element. This is a
     * handle-only method.
     *
     * @return the innermost resource enclosing this element, or
     *         <code>null</code>
     *
     */
    IResource getResource();

    // /**
    // * Returns the smallest underlying resource that contains this element, or
    // * <code>null</code> if this element is not contained in a resource.
    // *
    // * @return the underlying resource, or <code>null</code> if none
    // * @throws ErlModelException
    // * if this element does not exist or if an exception occurs
    // * while accessing its underlying resource
    // */
    // IResource getUnderlyingResource() throws ErlModelException;

    /**
     * Returns whether this Erlang element is read-only. An element is read-only
     * if its structure cannot be modified by the Erlang model.
     * <p>
     * Note this is different from IResource.isReadOnly().
     * <p>
     * This is a handle-only method.
     *
     * @return <code>true</code> if this element is read-only
     */
    boolean isReadOnly();

    /**
     * Returns whether the structure of this element is known. For example, for
     * a compilation unit that could not be parsed, <code>false</code> is
     * returned. If the structure of an element is unknown, navigations will
     * return reasonable defaults. For example, <code>getChildren</code> will
     * return an empty collection.
     * <p>
     * Note: This does not imply anything about consistency with the underlying
     * resource/buffer contents.
     * </p>
     *
     * @return <code>true</code> if the structure of this element is known
     * @throws ErlModelException
     *             if this element does not exist or if an exception occurs
     *             while accessing its corresponding resource
     */
    boolean isStructureKnown() throws ErlModelException;

    void resourceChanged(IResourceDelta delta);

    // static final int VISIT_REFERENCED = 0x0001;
    // static final int VISIT_EXTERNALS = 0x0002;
    enum AcceptFlags {
        LEAFS_ONLY, CHILDREN_FIRST
    }

    /**
     * The good old visitor pattern
     *
     * @param visitor
     * @param flags
     * @param leafKind
     * @throws ErlModelException
     */
    void accept(IErlElementVisitor visitor, Set<AcceptFlags> flags,
            ErlElementKind leafKind) throws ErlModelException;

    /**
     * Return the file path of the underlying element, i.e. if it's a module,
     * the file path to the element being edited
     *
     * @return path or null if not a file-based element
     */
    String getFilePath();

    void clearCaches();

    String toStringWithAncestors();

    // /////////////////////////////////////////////

    /**
     * Returns the immediate children of this element. Unless otherwise
     * specified by the implementing element, the children are in no particular
     * order.
     *
     * @exception ErlModelException
     *                if this element does not exist or if an exception occurs
     *                while accessing its corresponding resource
     * @return the immediate children of this element
     */
    List<IErlElement> getChildren() throws ErlModelException;

    int getChildCount();

    /**
     * Returns whether this element has one or more immediate children. This is
     * a convenience method, and may be more efficient than testing whether
     * <code>getChildren</code> is an empty array.
     *
     * @exception ErlModelException
     *                if this element does not exist or if an exception occurs
     *                while accessing its corresponding resource
     * @return true if the immediate children of this element, false otherwise
     */
    boolean hasChildren();

    List<IErlElement> getChildrenOfKind(ErlElementKind... kind) throws ErlModelException;

    boolean hasChildrenOfKind(ErlElementKind... kind);

    IErlElement getChildNamed(String s);

    IErlElement getChildWithResource(IResource rsrc);

    void addChild(IErlElement child);

    public void setChildren(final Collection<? extends IErlElement> children);

    void removeChild(IErlElement e);

    // //////////////////////

    /**
     * Closes this element and its buffer (if any). Closing an element which is
     * not open has no effect.
     *
     * <p>
     * Note: although <code>close</code> is exposed in the API, clients are not
     * expected to open and close elements - the Erlang model does this
     * automatically as elements are accessed.
     *
     * @exception ErlModelException
     *                if an error occurs closing this element
     */
    void close() throws ErlModelException;

    /**
     * Returns <code>true</code> if this element is open and:
     * <ul>
     * <li>its buffer has unsaved changes, or
     * <li>one of its descendants has unsaved changes, or
     * <li>a working copy has been created on one of this element's children and
     * has not yet destroyed
     * </ul>
     *
     * @exception ErlModelException
     *                if this element does not exist or if an exception occurs
     *                while accessing its corresponding resource.
     * @return <code>true</code> if this element is open and:
     *         <ul>
     *         <li>its buffer has unsaved changes, or
     *         <li>one of its descendants has unsaved changes, or
     *         <li>a working copy has been created on one of this element's
     *         children and has not yet destroyed
     *         </ul>
     */
    boolean hasUnsavedChanges() throws ErlModelException;

    /**
     * Returns whether the element is consistent with its underlying resource or
     * buffer. The element is consistent when opened, and is consistent if the
     * underlying resource or buffer has not been modified since it was last
     * consistent.
     *
     * <p>
     * NOTE: Child consistency is not considered. For example, a package
     * fragment responds <code>true</code> when it knows about all of its
     * compilation units present in its underlying folder. However, one or more
     * of the compilation units could be inconsistent.
     *
     * @exception ErlModelException
     *                if this element does not exist or if an exception occurs
     *                while accessing its corresponding resource.
     * @return true if the element is consistent with its underlying resource or
     *         buffer, false otherwise.
     * @see IErlElement#makeConsistent(IProgressMonitor)
     */
    boolean isConsistent() throws ErlModelException;

    /**
     * Returns whether this ErlElement is open. This is a handle-only method.
     *
     * @return true if this ErlElement is open, false otherwise
     */
    boolean isOpen();

    /**
     * Makes this element consistent with its underlying resource or buffer by
     * updating the element's structure and properties as necessary.
     * <p>
     *
     * @param progress
     *            the given progress monitor
     * @exception ErlModelException
     *                if the element is unable to access the contents of its
     *                underlying resource. Reasons include:
     *                <ul>
     *                <li>This Erlang element does not exist
     *                (ELEMENT_DOES_NOT_EXIST) </li>
     *                </ul>
     * @see IErlElement#isConsistent()
     */
    void makeConsistent(IProgressMonitor progress) throws ErlModelException;

    /**
     * Opens this element and all parent elements that are not already open. For
     * compilation units, a buffer is opened on the contents of the underlying
     * resource.
     *
     * <p>
     * Note: although <code>open</code> is exposed in the API, clients are not
     * expected to open and close elements - the Erlang model does this
     * automatically as elements are accessed.
     *
     * @param progress
     *            the given progress monitor
     * @exception ErlModelException
     *                if an error occurs accessing the contents of its
     *                underlying resource. Reasons include:
     *                <ul>
     *                <li>This Erlang element does not exist
     *                (ELEMENT_DOES_NOT_EXIST) </li>
     *                </ul>
     */
    void open(IProgressMonitor progress) throws ErlModelException;

    /**
     * Saves any changes in this element's buffer to its underlying resource via
     * a workspace resource operation. This has no effect if the element has no
     * underlying buffer, or if there are no unsaved changed in the buffer.
     * <p>
     * The <code>force</code> parameter controls how this method deals with
     * cases where the workbench is not completely in sync with the local file
     * system. If <code>false</code> is specified, this method will only attempt
     * to overwrite a corresponding file in the local file system provided it is
     * in sync with the workbench. This option ensures there is no unintended
     * data loss; it is the recommended setting. However, if <code>true</code>
     * is specified, an attempt will be made to write a corresponding file in
     * the local file system, overwriting any existing one if need be. In either
     * case, if this method succeeds, the resource will be marked as being local
     * (even if it wasn't before).
     * <p>
     * As a result of this operation, the element is consistent with its
     * underlying resource or buffer.
     *
     * @param progress
     *            the given progress monitor
     * @param force
     *            it controls how this method deals with cases where the
     *            workbench is not completely in sync with the local file system
     * @exception ErlModelException
     *                if an error occurs accessing the contents of its
     *                underlying resource. Reasons include:
     *                <ul>
     *                <li>This Erlang element does not exist
     *                (ELEMENT_DOES_NOT_EXIST) </li> <li>This Erlang element is
     *                read-only (READ_ONLY)</li>
     *                </ul>
     */
    void save(IProgressMonitor progress, boolean force) throws ErlModelException;

}
