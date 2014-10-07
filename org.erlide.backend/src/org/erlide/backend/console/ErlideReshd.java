package org.erlide.backend.console;

import org.erlide.backend.BackendUtils;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd implements ShellService {

    @Override
    public OtpErlangPid start(final IOtpNodeProxy nodeProxy) {
        try {
            final OtpErlangObject r = nodeProxy.getOtpRpc().call("erlide_shell", "start",
                    "p", nodeProxy.getEventPid());
            final OtpErlangPid server = (OtpErlangPid) BackendUtils.ok(r);
            return server;
        } catch (final RpcException e) {
            return null;
        }
    }
}
