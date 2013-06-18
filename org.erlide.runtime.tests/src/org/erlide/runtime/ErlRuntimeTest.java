package org.erlide.runtime;

import java.util.Arrays;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.util.Asserts;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlRuntimeTest {

    private static final String OTP_HOME = "c:/apps/erl5.9.3/";

    @Test
    public void runtimeProcessStartsAndIsAvailableForRpc() {
        final RuntimeInfoCatalog cat = new RuntimeInfoCatalog();
        cat.initializeRuntimesList();
        Asserts.isTrue(!cat.getRuntimes().isEmpty());
        final RuntimeInfo info = cat.getRuntimes().iterator().next();
        Asserts.isTrue(info != RuntimeInfo.NO_RUNTIME_INFO,
                "no default info found");

        final RuntimeData data = new RuntimeData(info, "run");
        data.setNodeName("etest");
        data.setLongName(false);
        data.setCookie("c");

        final ErlRuntime runtime = new ErlRuntime(data);
        System.out.println(Arrays.toString(data.getCmdLine()));
        final Process process = runtime.getProcess();
        Asserts.isNotNull(process);
        try {
            int val;
            try {
                val = process.exitValue();
            } catch (final IllegalThreadStateException e) {
                val = -1;
            }
            Asserts.isTrue(val == -1, "process exited " + val);
            Asserts.isTrue(runtime.isRunning(), "not running");
            runtime.connectRetry();
            runtime.connect();
            Asserts.isTrue(runtime.isAvailable(), "not available");
            final IRpcSite site = runtime.getRpcSite();
            OtpErlangObject r;
            try {
                r = site.call("erlang", "now", "");
            } catch (final RpcException e) {
                r = null;
            }
            Asserts.isNotNull(r);

        } finally {
            process.destroy();
        }
    }

}
