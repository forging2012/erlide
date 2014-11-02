package org.erlide.core.internal.builder;

public enum BuildPhase {
    CLEAN(5), COMPILE(65), EUNIT_COMPILE(5), EUNIT(20), XREF(5);

    private int work;

    BuildPhase(final int work) {
        this.work = work;
    }

    public int getWork() {
        return work;
    }
}
