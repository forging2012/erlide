package org.erlide.engine.services.edoc;

import java.util.Collection;
import java.util.Map;

import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface EdocExportService extends ErlangService {

    public abstract void files(Collection<String> files,
            Map<String, OtpErlangObject> options) throws RpcException;

}
