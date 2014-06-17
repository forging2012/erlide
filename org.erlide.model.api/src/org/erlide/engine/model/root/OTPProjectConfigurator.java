package org.erlide.engine.model.root;

import org.eclipse.core.runtime.IPath;

public class OTPProjectConfigurator implements IProjectConfigurator {

    @Override
    public ErlangProjectProperties getConfiguration(final IPath baseDir) {
        final ErlangProjectProperties result = new ErlangProjectProperties();
        result.copyFrom(ErlangProjectProperties.DEFAULT);
        return result;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) {
    }

}
