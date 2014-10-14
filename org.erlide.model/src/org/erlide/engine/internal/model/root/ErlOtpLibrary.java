package org.erlide.engine.internal.model.root;

import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlOtpLibrary;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public class ErlOtpLibrary extends ErlLibrary implements IErlOtpLibrary {

    private final RuntimeVersion version;

    public ErlOtpLibrary(final RuntimeVersion version, final IErlElement parent) {
        super(version.toString(), parent);
        this.version = version;
    }

    @Override
    public RuntimeVersion getVersion() {
        return version;
    }

}
