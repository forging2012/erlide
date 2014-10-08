/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.rpc;

public class RpcNoTargetException extends RpcException {

    private static final long serialVersionUID = 1L;

    public RpcNoTargetException() {
        this("");
    }

    public RpcNoTargetException(final Exception e) {
        super(e);
    }

    public RpcNoTargetException(final String string) {
        super(string);
    }

}
