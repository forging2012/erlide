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
package org.erlide.engine.internal.model.erlang;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.internal.model.root.ErlElement;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.ISourceReference;
import org.erlide.engine.model.root.IErlElement;

import com.google.common.base.Objects;

/**
 * Abstract class for Erlang elements which implement ISourceReference.
 */
public abstract class SourceRefElement extends ErlElement implements ISourceReference {

    protected int fSourceRangeOffset;
    protected int fSourceRangeLength;
    protected int lineStart, lineEnd;

    protected SourceRefElement(final IErlElement parent, final String name) {
        super(parent, name);
    }

    /**
     * Returns a new element info for this element.
     */
    protected Object createElementInfo() {
        return null; // not used for source ref elements
    }

    /*
     * @see ErlElement#generateInfos
     */
    @Override
    public synchronized void open(final IProgressMonitor pm) throws ErlModelException {
        final ErlElement ErlElementParent = (ErlElement) getErlElementParent();
        if (ErlElementParent == null) {
            return;
        }

        ErlElementParent.open(pm);
    }

    /**
     * Elements within compilation units and class files have no corresponding
     * resource.
     *
     * @see IErlElement
     */
    @Override
    public IResource getCorrespondingResource() {
        // if (!exists()) {
        // throw newNotPresentException();
        // }
        return null;
    }

    /**
     * Return the first instance of IErlElement in the hierarchy of this type
     * (going up the hierarchy from this type);
     */
    @Override
    public IErlElement getErlElementParent() {
        final IErlElement parent = getParent();
        while (parent != null) {
            {
                return parent;
            }
        }
        return null;
    }

    /**
     * @see ISourceReference
     */
    // public String getSource() throws ErlModelException {
    // // final IErlElement ErlElement = getErlElementParent();
    // // final IBuffer buffer = ErlElement.getBuffer();
    // // if (buffer == null) {
    // // return null;
    // // }
    // // final ISourceRange range = getSourceRange();
    // // final int offset = range.getOffset();
    // // final int length = range.getLength();
    // // if (offset == -1 || length == 0) {
    // // return null;
    // // }
    // // try {
    // // return buffer.getText(offset, length);
    // // } catch (final RuntimeException e) {
    // // return null;
    // // }
    // return "";
    // }
    /**
     * @see ISourceReference
     */
    @Override
    public ISourceRange getSourceRange() {
        return new SourceRange(fSourceRangeOffset, fSourceRangeLength);
    }

    // /**
    // * @see IErlElement
    // */
    // public IResource getUnderlyingResource() throws ErlModelException {
    // if (!exists()) {
    // throw newNotPresentException();
    // }
    // return getParent().getUnderlyingResource();
    // }

    /**
     * @see IErlElement
     */
    @Override
    public boolean hasChildren() {
        synchronized (getModelLock()) {
            return internalGetChildren().size() > 0;
        }
    }

    public void setSourceRangeOffset(final int offset) {
        fSourceRangeOffset = offset;
    }

    public void setSourceRangeLength(final int length) {
        fSourceRangeLength = length;
    }

    public void setLineStart(final int lineStart) {
        this.lineStart = lineStart;
    }

    @Override
    public int getLineStart() {
        return lineStart;
    }

    public void setLineEnd(final int lineEnd) {
        this.lineEnd = lineEnd;
    }

    @Override
    public int getLineEnd() {
        return lineEnd;
    }

    @Override
    public boolean equals(final Object o) {
        if (!super.equals(o) || !(o instanceof SourceRefElement)) {
            return false;
        }
        final SourceRefElement r = (SourceRefElement) o;
        return fSourceRangeOffset == r.fSourceRangeOffset
                && fSourceRangeLength == r.fSourceRangeLength;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(super.hashCode(), fSourceRangeOffset, fSourceRangeLength);
    }

    @Override
    public String getSource() throws ErlModelException {
        throw new UnsupportedOperationException();
    }
}
