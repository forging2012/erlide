/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.erlang.IErlAttribute;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author Vlad Dumitrescu
 */
public class ErlAttribute extends ErlMember implements IErlAttribute {

    private final OtpErlangObject fValue;
    private final String fExtra;

    /**
     * @param parent
     * @param name
     */
    public ErlAttribute(final IErlElement parent, final String name,
            final OtpErlangObject value, final String extra) {
        super(parent, name);
        fValue = value;
        fExtra = extra;
    }

    /**
     * @see org.erlide.engine.model.root.IErlElement#getKind()
     */
    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.ATTRIBUTE;
    }

    @Override
    public OtpErlangObject getValue() {
        return fValue;
    }

    @Override
    public String toString() {
        String sval;
        if (fValue != null) {
            sval = ": " + fValue.toString(); // pp(fValue);
        } else if (fExtra != null) {
            sval = ": " + fExtra;
        } else {
            sval = "";
        }
        return getName() + sval;
    }

    @Override
    public String getExtra() {
        return fExtra;
    }
}
