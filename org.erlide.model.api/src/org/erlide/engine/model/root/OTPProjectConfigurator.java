package org.erlide.engine.model.root;

import org.eclipse.core.runtime.IPath;

public class OTPProjectConfigurator implements IProjectConfigurator {

    @Override
    public ErlangProjectProperties getConfiguration(final IPath baseDir) {
        return ErlangProjectProperties.DEFAULT;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) {
    }

}
