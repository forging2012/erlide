/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.engine.internal.model.erlang;

import java.util.List;

import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlFunctionClause;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlFunctionClause extends ErlMember implements IErlFunctionClause {

    final String head;
    final List<String> parameters;

    public ErlFunctionClause(final IErlElement parent, final String name,
            final String head, final OtpErlangList parameters) {
        super(parent, name);
        this.head = head;
        this.parameters = getParameters(parameters);
    }

    public static List<String> getParameters(final OtpErlangList parameters) {
        return Util.asStringList(parameters);
    }

    @Override
    public String getHead() {
        return head;
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.CLAUSE;
    }

    /**
     * @param arguments
     *            the arguments to set
     */
    // public void setArguments(final String arguments) {
    // this.arguments = arguments;
    // }
    /**
     * @param guards
     *            the guards to set
     */
    // public void setGuards(final OtpErlangList guards) {
    // this.guards = guards;
    // }
    @Override
    public String toString() {
        return head;
    }

    @Override
    public String getFunctionName() {
        final IErlElement element = getParent();
        return element.getName();
    }

    @Override
    public int getArity() {
        final IErlFunction f = (IErlFunction) getParent();
        return f.getArity();
    }

    @Override
    public List<String> getParameters() {
        return parameters;
    }
}
