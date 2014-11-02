/*******************************************************************************
 * Copyright (c) 2010 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.runtime.api.ErlRuntimeReporter;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangLogEventHandler;
import org.erlide.runtime.events.LogEventHandler;
import org.erlide.runtime.internal.EventParser;
import org.erlide.runtime.internal.LocalNodeCreator;
import org.erlide.runtime.internal.rpc.OtpRpc;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.google.common.collect.Lists;
import com.google.common.eventbus.DeadEvent;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.google.common.util.concurrent.AbstractExecutionThreadService;

public class OtpNodeProxy extends AbstractExecutionThreadService implements IOtpNodeProxy {
    private static final String COULD_NOT_CONNECT = "Could not connect to ";

    private static final int MAX_RETRIES = 15;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "400"));

    private final RuntimeData data;

    private OtpNode localNode;
    private OtpMbox eventMBox;
    private IOtpRpc otpRpc;
    private final EventBus eventBus;
    private final EventParser eventHelper;
    private volatile boolean stopped;
    private final List<Procedure1<Boolean>> statusHandlers = Lists.newArrayList();

    static final boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));
    public static final long POLL_INTERVAL = 100;

    public OtpNodeProxy(final RuntimeData data) {
        this.data = data;

        eventHelper = new EventParser();
        final String nodeName = getNodeName();
        eventBus = new EventBus(nodeName);
        eventBus.register(this);
        registerEventListener(new LogEventHandler(nodeName));
        registerEventListener(new ErlangLogEventHandler(nodeName));

        addListener(new OtpNodeProxyListener(), executor());
    }

    @Override
    protected void startUp() throws Exception {
        localNode = LocalNodeCreator.startLocalNode(this, data.getCookie(),
                data.hasLongName());
        registerStatusHandler();

        eventMBox = createMbox("rex");
        otpRpc = new OtpRpc(getLocalNode(), getNodeName());
        addStatusHandler(new Procedure1<Boolean>() {
            @Override
            public void apply(final Boolean up) {
                ErlLogger.debug("RPC " + (up ? "up" : "down") + " for "
                        + data.getNodeName());
                otpRpc.setConnected(up);
            }
        });
        connect();
        stopped = false;
    }

    @Override
    public boolean connect() {
        boolean connected = doConnect();
        if (!waitForCodeServer()) {
            triggerShutdown();
            ErlLogger.error(COULD_NOT_CONNECT, getNodeName());
            connected = false;
        }
        return connected;
    }

    private void registerStatusHandler() {
        final OtpNodeStatus watcher = new OtpNodeStatus() {
            @Override
            public void remoteStatus(final String peer, final boolean up,
                    final Object info) {
                if (peer.equals(getNodeName())) {
                    for (final Procedure1<Boolean> handler : statusHandlers) {
                        try {
                            handler.apply(up);
                        } catch (final Exception e) {
                            // ignore
                        }
                    }
                }
            }
        };
        localNode.registerStatusHandler(watcher);
    }

    @Override
    public void addStatusHandler(final Procedure1<Boolean> handler) {
        statusHandlers.add(handler);
    }

    @Override
    protected void shutDown() throws Exception {
        localNode.close();
    }

    @Override
    protected void triggerShutdown() {
        stopped = true;
    }

    @Override
    protected void run() throws Exception {
        final OtpMbox eventBox = eventMBox;
        do {
            receiveEventMessage(eventBox);
        } while (!stopped);
    }

    private void receiveEventMessage(final OtpMbox eventBox) throws OtpErlangExit {
        OtpErlangObject msg = null;
        try {
            msg = eventBox.receive(POLL_INTERVAL);
            final ErlEvent busEvent = eventHelper.parse(msg, this);
            if (busEvent != null) {
                if (DEBUG) {
                    ErlLogger.debug("MSG: %s", "[" + busEvent.getSender() + "::"
                            + busEvent.getTopic() + ": " + busEvent.getEvent() + "]");
                }
                eventBus.post(busEvent);
            }
        } catch (final OtpErlangExit e) {
            ErlLogger.error(e);
            throw e;
        } catch (final OtpErlangDecodeException e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public OtpNode getLocalNode() {
        return localNode;
    }

    @Override
    public String getNodeName() {
        return data.getQualifiedNodeName();
    }

    @Override
    public OtpMbox createMbox(final String name) {
        return localNode.createMbox(name);
    }

    @Override
    public OtpMbox createMbox() {
        return localNode.createMbox();
    }

    @Override
    public OtpErlangPid getEventPid() {
        return eventMBox.self();
    }

    @Override
    public RuntimeVersion getVersion() {
        return data.getRuntimeInfo().getVersion();
    }

    @Override
    public String getOtpHome() {
        return data.getRuntimeInfo().getOtpHome();
    }

    @Override
    public void dispose() {
        triggerShutdown();
    }

    @Override
    public IOtpRpc getOtpRpc() {
        try {
            awaitTerminated(20, TimeUnit.MILLISECONDS);
            return null;
        } catch (final TimeoutException e) {
            awaitRunning();
            return otpRpc;
        }
    }

    @Override
    public void registerEventListener(final Object handler) {
        eventBus.register(handler);
    }

    @Override
    public void unregisterEventListener(final Object handler) {
        try {
            eventBus.unregister(handler);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }

    @Override
    protected String serviceName() {
        return getClass().getSimpleName() + " " + getNodeName();
    }

    private boolean doConnect() {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer... ");

        final boolean connected = pingPeer();
        if (!connected) {
            ErlLogger.error(COULD_NOT_CONNECT, getNodeName());
        }
        return connected;
    }

    private boolean pingPeer() {
        int tries = MAX_RETRIES;
        boolean ok = false;
        while (!ok && tries > 0) {
            ok = localNode.ping(getNodeName(), RETRY_DELAY + (MAX_RETRIES - tries)
                    * RETRY_DELAY % 3);
            tries--;
        }
        return ok;
    }

    private boolean waitForCodeServer() {
        try {
            OtpErlangObject r;
            int i = 30;
            boolean gotIt = false;
            do {
                r = otpRpc.call("erlang", "whereis", "a", "code_server");
                gotIt = !(r instanceof OtpErlangPid);
                if (!gotIt) {
                    try {
                        Thread.sleep(POLL_INTERVAL);
                    } catch (final InterruptedException e) {
                    }
                }
                i--;
            } while (gotIt && i > 0);
            if (gotIt) {
                ErlLogger
                        .error("code server did not start in time for %s", getNodeName());
                return false;
            }
            return true;
        } catch (final Exception e) {
            ErlLogger.error("error starting code server for %s: %s", getNodeName(),
                    e.getMessage());
            return false;
        }
    }

    @Subscribe
    public void deadEventHandler(final DeadEvent dead) {
        ErlLogger.warn("Dead event: " + dead + " in runtime " + getNodeName());
    }

    private class OtpNodeProxyListener extends Listener {
        public OtpNodeProxyListener() {
        }

        @Override
        public void terminated(final State from) {
            ErlLogger.debug(String.format("Node %s terminated", getNodeName()));
            dispose();
            final ErlRuntimeReporter reporter = new ErlRuntimeReporter(data.isInternal(),
                    data.isReportErrors());
            if (getExitCode() > 0) {
                reporter.reportRuntimeDown(getNodeName());
            }
        }

        @Override
        public void failed(final State from, final Throwable failure) {
            ErlLogger.error(failure);
        }

    }

    public int getExitCode() {
        return -1;
    }

    @Override
    public void startAndWait() {
        startAsync();
        awaitRunning();
    }

}
