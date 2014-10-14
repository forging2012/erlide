package org.erlide.engine.internal.model.root;

import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlOtpLibrary;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public class ErlOtpLibrary extends ErlLibrary implements IErlOtpLibrary {

    private final RuntimeVersion version;

    public ErlOtpLibrary(final IErlElement parent, final RuntimeVersion version) {
        super(parent, version.toString());
        this.version = version;
    }

    @Override
    public RuntimeVersion getVersion() {
        return version;
    }

}
