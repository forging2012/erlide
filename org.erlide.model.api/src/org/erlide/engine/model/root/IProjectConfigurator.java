package org.erlide.engine.model.root;

import org.eclipse.core.runtime.IPath;

public interface IProjectConfigurator {

    ErlangProjectProperties getConfiguration(IPath iPath);

    void setConfiguration(final ErlangProjectProperties info);

}
