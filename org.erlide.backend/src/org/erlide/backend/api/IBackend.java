package org.erlide.backend.api;

import java.io.IOException;

import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.runtime.ErtsProcess;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.IDisposable;

public interface IBackend extends IDisposable, IPluginCodeLoader, IProjectCodeLoader {

    String getName();

    BackendData getData();

    IOtpRpc getOtpRpc();

    IOtpNodeProxy getNodeProxy();

    boolean isRunning();

    boolean isDebugging();

    IBackendShell getShell(String string);

    void initialize(CodeContext context, IBackendManager backendManager);

    void input(final String s) throws IOException;

    ErtsProcess getErtsProcess();

}
