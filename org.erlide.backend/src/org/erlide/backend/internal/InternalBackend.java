/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.internal;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.backend.api.BackendData;
import org.erlide.runtime.ErtsProcess;
import org.erlide.runtime.api.IOtpNodeProxy;

public class InternalBackend extends Backend {

    public InternalBackend(final BackendData data,
            final @NonNull IOtpNodeProxy nodeProxy, final ErtsProcess erts) {
        super(data, nodeProxy, erts);
    }

    @Override
    public synchronized void dispose() {
        getData().setLaunch(null);
        super.dispose();
    }

    @Override
    protected boolean shouldRestart() {
        final boolean result = isCrashed() && getErtsProcess() != null;
        System.out.println(getName() + " >> RESTART? " + result);
        return result;
    }

}
