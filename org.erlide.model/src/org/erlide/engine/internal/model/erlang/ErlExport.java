package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.erlang.IErlExport;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

    private final String functions;

    public ErlExport(final IErlElement parent, final OtpErlangList functionList,
            final String functions) {
        super(parent, "export", functionList);
        this.functions = functions;
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.EXPORT;
    }

    @Override
    public String toString() {
        return getName() + ": " + functions;
    }

}
