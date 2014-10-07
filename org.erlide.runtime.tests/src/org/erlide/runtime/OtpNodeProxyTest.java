package org.erlide.runtime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.util.concurrent.Service.State;

public class OtpNodeProxyTest {

    private Process process;
    private OtpNodeProxy nodeProxy;
    private RuntimeInfo info;

    @Before
    public void prepareRuntime() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        assertThat("empty runtime list", !cat.getRuntimes().isEmpty());
        info = cat.getRuntimes().iterator().next();
        assertThat("no default info", info != RuntimeInfo.NO_RUNTIME_INFO);

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest" + System.currentTimeMillis());
        data.setLongName(false);
        data.setCookie("c");

        final ErtsProcess erts = new ErtsProcess(data);
        erts.startUp();
        nodeProxy = new OtpNodeProxy(data);
        nodeProxy.startAndWait();
        nodeProxy.addStatusHandler(new Procedure1<Boolean>() {
            @Override
            public void apply(final Boolean p) {
                nodeProxy.triggerShutdown();
            }
        });

        process = erts.getProcess();
        assertThat("beam process didn't start", process, is(not(nullValue())));
    }

    @After
    public void cleanupRuntime() {
        if (process != null) {
            process.destroy();
        }
        process = null;
        nodeProxy = null;
    }

    @Test
    public void runtimeProcessStartsAndIsAvailableForRpc() {
        int val;
        try {
            val = process.exitValue();
        } catch (final IllegalThreadStateException e) {
            val = -1;
        }
        assertThat("bad exit value", val, is(-1));
        assertThat("not running", nodeProxy.isRunning(), is(true));
        final IOtpRpc site = nodeProxy.getOtpRpc();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat("rpc", r, is(not(nullValue())));
        try {
            site.cast("erlang", "halt", "i", 0);
        } catch (final RpcException e1) {
        }
        expect(nodeProxy, process, 0, State.TERMINATED);
    }

    @Test
    public void shutdownIsDetected() {
        final IOtpRpc site = nodeProxy.getOtpRpc();
        try {
            site.cast("erlang", "halt", "i", 0);
        } catch (final RpcException e1) {
        }
        expect(nodeProxy, process, 0, State.TERMINATED);
    }

    @Test
    public void exitCodeIsDetected() {
        final IOtpRpc site = nodeProxy.getOtpRpc();
        try {
            site.cast("erlang", "halt", "i", 3);
        } catch (final RpcException e1) {
        }
        expect(nodeProxy, process, 3, State.TERMINATED);
    }

    @Test
    public void crashIsDetected() {
        process.destroy();
        final int code = SystemConfiguration.getInstance().isOnWindows() ? 1 : 143;
        expect(nodeProxy, process, code, State.TERMINATED);
    }

    @Test
    public void haltIsDetected() throws RpcException {
        nodeProxy.getOtpRpc().cast("erlang", "halt", "i", 136);
        expect(nodeProxy, process, 136, State.TERMINATED);
    }

    private void expect(final OtpNodeProxy aNodeProxy, final Process aProcess,
            final int code, final State state) {
        while (aNodeProxy.isRunning()) {
            try {
                Thread.sleep(OtpNodeProxy.POLL_INTERVAL);
            } catch (final InterruptedException e) {
            }
        }
        assertThat("state", aNodeProxy.state(), is(state));
        if (aProcess != null) {
            int val;
            try {
                val = aProcess.waitFor();
            } catch (final InterruptedException e) {
                val = -1;
            }
            assertThat("exit code", val, is(code));
        }
    }

    @Test
    public void nonManagedRuntimeWorks() {
        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName(nodeProxy.getNodeName());
        data.setLongName(false);
        data.setCookie("c");
        data.setManaged(false);

        final OtpNodeProxy runtime2 = new OtpNodeProxy(data);
        runtime2.startAsync();
        runtime2.awaitRunning();

        final IOtpRpc site = runtime2.getOtpRpc();
        OtpErlangObject r;
        try {
            r = site.call("erlang", "now", "");
        } catch (final RpcException e) {
            r = null;
        }
        assertThat("rpc", r, is(not(nullValue())));
        try {
            runtime2.stopAsync();
            runtime2.awaitTerminated();
        } catch (final Throwable t) {
            ErlLogger.error(t);
        }
        assertThat("state", nodeProxy.state(), is(State.RUNNING));
    }
}
