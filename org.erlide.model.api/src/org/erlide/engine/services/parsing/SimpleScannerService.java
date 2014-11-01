package org.erlide.engine.services.parsing;

import java.util.List;

import org.erlide.engine.services.ErlangService;

public interface SimpleScannerService extends ErlangService {

    List<ErlToken> lightScanString(String string, int offset) throws ScannerException;

}
