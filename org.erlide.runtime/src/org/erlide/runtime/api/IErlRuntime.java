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
package org.erlide.runtime.api;

import org.erlide.util.IDisposable;

import com.ericsson.otp.erlang.OtpMbox;

public interface IErlRuntime extends IDisposable {

    void startAndWait();

    boolean isRunning();

    String getNodeName();

    IRpcSite getRpcSite();

    RuntimeData getRuntimeData();

    OtpMbox getEventMbox();

    Process getProcess();

    ErlSystemStatus getSystemStatus();

    void setSystemStatus(ErlSystemStatus msg);

    OtpMbox createMbox(String string);

    OtpMbox createMbox();

    void addShutdownCallback(IShutdownCallback callback);

    void registerEventListener(Object handler);

    void unregisterEventListener(Object handler);

}
