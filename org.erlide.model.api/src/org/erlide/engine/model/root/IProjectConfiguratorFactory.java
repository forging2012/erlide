package org.erlide.engine.model.root;

import java.io.File;

import org.erlide.engine.services.ErlangService;

public interface IProjectConfiguratorFactory extends ErlangService {

    public abstract IProjectConfigurator getConfig(ProjectConfigType configType,
            IErlProject project);

    public abstract IProjectConfigurator getConfig(ProjectConfigType configType,
            File directory);

}
