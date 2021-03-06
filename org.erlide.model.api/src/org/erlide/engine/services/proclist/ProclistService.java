package org.erlide.engine.services.proclist;

import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.api.IOtpRpc;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface ProclistService extends ErlangService {

    public abstract void processListInit(IOtpRpc b);

    public abstract OtpErlangList getProcessList(IOtpRpc b);

    public abstract OtpErlangObject getProcessInfo(IOtpRpc b, OtpErlangPid pid);

}
