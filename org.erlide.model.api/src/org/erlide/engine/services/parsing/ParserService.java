package org.erlide.engine.services.parsing;

import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.ErlangService;

public interface ParserService extends ErlangService {

    // TODO this API is not good at all, better split it in multiple methods
    // according to which parameters are used

    boolean parse(final IErlModule module, final String scannerName,
            final boolean initialParse, final String path, String initialText,
            boolean updateSearchServer);

}
