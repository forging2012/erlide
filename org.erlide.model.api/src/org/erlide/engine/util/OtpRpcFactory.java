package org.erlide.engine.util;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.IOtpRpcProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.services.ExtensionUtils;

public class OtpRpcFactory {

    private static IOtpRpcProvider provider;

    public static IOtpRpc getOtpRpc(final RuntimeVersion version) {
        return getOtpRpcProvider().get(version);
    }

    public static IOtpRpc getOtpRpcForProject(final IErlProject project) {
        return getOtpRpcProvider().get(project.getName());
    }

    public static IOtpRpc getOtpRpc() {
        return getOtpRpcProvider().get();
    }

    private static IOtpRpcProvider getOtpRpcProvider() {
        if (provider == null) {
            provider = ExtensionUtils.getSingletonExtension("org.erlide.backend.backend",
                    IOtpRpcProvider.class);
        }
        return provider;
    }

}
