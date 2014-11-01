package org.erlide.engine.services.importer;

import java.util.List;

import org.erlide.engine.services.ErlangService;

public interface ImportService extends ErlangService {

    /**
     * Filter import parameters for newly imported erlang project
     *
     * @param prefix
     * @param importSources
     * @param prefix
     *
     * @return
     */
    public abstract ErlProjectImport importProject(String prefix,
            List<String> importSources);

}
