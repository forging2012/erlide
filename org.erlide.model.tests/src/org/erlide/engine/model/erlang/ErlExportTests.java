package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.junit.Before;
import org.junit.Test;

public class ErlExportTests extends WorkspaceTest {

    protected IErlModule module;
    protected IErlModule module2;
    protected IErlFunction functionA;
    protected IErlFunction functionB;
    protected IErlFunction functionC;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");

        module = getErlModule("xx.erl");
        module2 = getErlModule("zz.erl");
        module2.open(null);
        final List<IErlElement> functions = module2
                .getChildrenOfKind(ErlElementKind.FUNCTION);
        functionA = (IErlFunction) functions.get(0);
        functionB = (IErlFunction) functions.get(1);
        functionC = (IErlFunction) functions.get(2);
    }

    // public boolean hasFunction(final ErlangFunction f);
    @Test
    public void hasFunction() throws Exception {
        module.open(null);
        final List<IErlElement> childrenOfKind = module2
                .getChildrenOfKind(ErlElementKind.EXPORT);
        final IErlElement element = childrenOfKind.get(0);
        final IErlExport export = (IErlExport) element;
        assertTrue(export.hasFunction(functionA.getFunction()));
        assertTrue(export.hasFunction(functionB.getFunction()));
        assertFalse(export.hasFunction(functionC.getFunction()));
    }

}
